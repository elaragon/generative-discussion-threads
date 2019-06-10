library(dplyr)

path = '../../data/meneame'
df.trees <- data.frame()
files <- dir(path, pattern = ".csv",full.names=TRUE)

for (file in files) {
  df.thread <- read.csv(file, header=FALSE, sep=",", stringsAsFactors = FALSE)
  names(df.thread) <- c('parent', 'depth', 'user', 'parent.user')
  df.thread$thread = gsub('(^meneame/|.csv$)','',file)
  df.thread$parent = df.thread$parent+1
  df.thread <- df.thread %>% mutate(post = row_number()+1, t = post-1) %>% as.data.frame

  # Correct wrong parents like https://www.meneame.net/m/actualidad/asi-son-votantes-cada-partido-segun-cifras-cis/c015#c-15
  df.thread$parent <- ifelse(df.thread$parent >= df.thread$post, 1, df.thread$parent)

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

  if ((!restriction.replytwice) & (!restriction.selfreply)) df.thread$train <- TRUE
  else df.thread$train <- FALSE
  df.trees <- rbind(df.trees, df.thread)
}

# add grandparent info
# with the following code if I have A --> A --> A, the last A has grandparent == TRUE
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
      if(temp$user[t]==temp$parent.user[t]){
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
#df.trees <- mydf
#df.trees$grandparent[is.na(df.trees$grandparent)] <- FALSE

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

# add thread size
df.trees <- df.trees %>%
  group_by(thread) %>%
  mutate(thread.size = n()) %>%
  ungroup %>%
  as.data.frame

# add root
df.trees$root <- ifelse(df.trees$parent==1,1,0)

# save dataframe
save(df.trees, file = paste(path,'.df.trees.rda',sep=''))
