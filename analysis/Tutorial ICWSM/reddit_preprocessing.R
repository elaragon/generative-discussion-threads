library(dplyr)
library(jsonlite)

# read submissions
submissions_path <- '../../data/gameofthrones/RS_2018-all'
df.submissions <- fromJSON(paste("[",paste(readLines(submissions_path),collapse=","),"]"))
df.submissions <- df.submissions %>%
  select(author, id) %>%
  as.data.frame
df.submissions$id = paste('t3_', df.submissions$id, sep = '')

# read comments
comments_path <- '../../data/gameofthrones/RC_2018-all'
df.trees <- fromJSON(paste("[",paste(readLines(comments_path),collapse=","),"]"))
df.trees <- df.trees %>%
  select(id, created_utc, author, parent_id, link_id) %>%
  as.data.frame
df.trees$id = paste('t1_', df.trees$id, sep = '')

# remove threads with deleted users and bots
# (this is needed for Aragón et al 2017 but could skipped for Gomez et al. 2013)
cat(length(unique(df.trees$parent_id)), ' threads in the dump\n')
#threads_to_remove = (unique((df.trees %>% filter(author=='[deleted]' | author=='AutoModerator'))$link_id))
threads_to_remove = unique((df.trees %>% filter(author=='[deleted]'))$link_id)
df.trees <- df.trees %>% filter(!link_id %in% threads_to_remove)
cat(length(unique(df.trees$parent_id)), ' threads in the dump without [deleted] or AutoModerator users\n')

# add post: integer incremental id per thread
# add t: integer indicating the discrete time
df.trees <- df.trees %>%
  group_by(link_id) %>%
  arrange(as.character(created_utc)) %>%
  mutate(post = row_number()+1,
         t = post-1) %>%
  as.data.frame

# Create temporary hash dataframe (id,thread,post) to create the parent field with a merge
temp1 <- df.trees %>%
  select(link_id,id, post) %>%
  as.data.frame
temp2 <- df.trees %>%
  select(link_id,id,parent_id) %>%
  as.data.frame
mergedData <- right_join(temp1, temp2, by = c("link_id" = "link_id", "id" = "parent_id")) %>%
  select(id, post, id.y,link_id)
names(mergedData) <- c("parent_id", "parent", "id", "link_id")
mergedData$parent[is.na(mergedData$parent)] <- 1
df.trees <- merge(df.trees, mergedData, by=c("id","link_id"), all = T)
df.trees <- df.trees %>%
  select(id,created_utc, author, parent_id.x, link_id, post, t, parent) %>%
  as.data.frame

# rename some columns
names(df.trees)[names(df.trees)=="parent_id.x"] <- "parent_id"
names(df.trees)[names(df.trees)=="link_id"] <- "thread"
names(df.trees)[names(df.trees)=="author"] <- "user"
names(df.trees)[names(df.trees)=="created_utc"] <- "date"

# add parent user
mydf <- data.frame(matrix(ncol = length(names(df.trees)), nrow = 0))
for(th in unique(df.trees$thread)){
  temp <- df.trees %>% filter(thread == th)
  parent.users <- temp[match(temp$parent, temp$post),]$user
  temp <- temp %>% mutate(parent.user=parent.users)
  mydf <- rbind(mydf,temp)
}
df.trees <- mydf

# add grandparent info
# with the following code if I have A --> A --> A , the last A has grandparent == TRUE
# even if is also the same user replying to her own comments
mydf <- data.frame(matrix(ncol = length(names(df.trees)), nrow = 0))
for(th in unique(df.trees$thread)){
  temp <- df.trees %>% filter(thread == th)
  grandparents <- sapply(seq_along(temp$parent), function(t){
    if(temp$parent[t]==1) {
      # reply to root have no grandparent
      FALSE
    }
    else{
      if(temp$user[t]==temp$user[which(temp$post == temp$parent[t])]){
        #if the user is replying to herself, grandparent = F
        FALSE
      }else{
        #original
        temp$user[t]==temp$parent.user[which(temp$post == temp$parent[t])]
      }
    }
  })
  temp$grandparent <- ifelse(grandparents,1,0)
  mydf <- rbind(mydf,temp)
}
df.trees <- mydf
df.trees$grandparent[is.na(df.trees$grandparent)] <- FALSE
df.trees$parent.user <- as.character(df.trees$parent.user)
df.trees$parent.user[is.na(df.trees$parent.user)]<- "none"

# add popularity (parent degree)
mydf <- data.frame(matrix(ncol = length(names(df.trees)), nrow = 0))
for(th in unique(df.trees$thread)){
  temp <- df.trees %>% filter(thread == th)
  if(length(temp$parent)==1){
    popularity <- 1
  }else{
    popularity <- c(1, sapply(2:length(temp$parent), function(t){
      1 + sum(temp$parent[1:(t-1)]==temp$parent[t])
    }))
  }
  temp$popularity <- popularity
  mydf <- rbind(mydf,temp)
}
df.trees <- mydf

# add lag
df.trees <- df.trees %>%
  mutate(lag = post - parent) %>%
  as.data.frame

#add thread size
df.trees <- df.trees %>%
  group_by(thread) %>%
  mutate(thread.size = n()) %>%
  ungroup %>%
  as.data.frame

# add depth
mydf <- data.frame(matrix(ncol = length(names(df.trees)), nrow = 0))
for(th in unique(df.trees$thread)){
  temp <- df.trees %>% filter(thread == th)
  for (t in temp$t)  {
    # reply to root have no grandparent
    if(temp$parent[t]==0){ temp$depth[t] <- 1 }
    else{ temp$depth[t] <- temp$depth[which(temp$post == temp$parent[t])] + 1 }
  }
  mydf <- rbind(mydf,temp)
}
df.trees <- mydf

# add root
df.trees$root <- ifelse(df.trees$parent==1,1,0)

df.trees$train <- FALSE
threads = unique(df.trees$thread)
for (th in threads) {
  df.thread <- df.trees %>% filter(thread==th)
  # a user can only reply once to a same comment
  # (this is needed for Aragón et al 2017 but could skipped for Gomez et al. 2013)
  restriction.replytwice <- FALSE
  users = unique(df.thread$user)
  for (u in users) {
    parents = (df.thread %>% filter(user==u))$parent
    restriction.replytwice = restriction.replytwice | (length(parents)!=length(unique(parents)))
  }
  # a user cannot self-reply a comment made by herself
  # (this is needed for Aragón et al 2017 but could skipped for Gomez et al. 2013)
  restriction.selfreply <- sum(sapply(seq(nrow(df.thread)), function(i) { ifelse((df.thread$user[i] == df.thread$parent.user[i])&&(i>1),1,0) }))> 0
  if ((!restriction.replytwice) & (!restriction.selfreply))   df.trees$train[df.trees$thread == th] <- TRUE
}

# save dataframe
save(df.trees, file = paste(comments_path,'.df.trees.rda',sep=''))
