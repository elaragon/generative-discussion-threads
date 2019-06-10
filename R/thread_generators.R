# Generates synthetic threads

library(igraph)
library(dplyr)
library(stringi)

#' @title Generate parents vector
#' @description parent vector is a tree encoded as a vector where a position i
#' encodes the parent of the node i
gen.parentsvector.Gomez2013 <- function(n=100, alpha=0, beta = 0, tau=1){

  if(n==1) { return(1) }

  likelihood <- 0
  # n-1 parents because root has no parent
  parents <- c()

  # Second post (1st after root) has no choice, always choses the root
  parents[1] <- 1

  # Start from the 3rd post (2nd after root), which arrives at t=2
  for (t in 2:(n-1)){
    betas <- c(beta, rep(0, t-1))
    # Note that the latest post has lag = tau
    lags <- t:1
    # We consider an undirected graph, and every existing node has degree equal to one initially
    popularities <- 1 + tabulate(parents, nbins=t)
    # but root has no outcoming link
    popularities[1] <- popularities[1] -1

    # Probability of choosing every node (only one is chosen)
    probs <- alpha * popularities + betas + tau^lags
    if(sum(probs) == 0) { probs = rep(1/t, t) }
    else { probs <- probs/sum(probs) }

    # Add new vertex attached to the chosen node
    parents[t] <- sample(length(probs), size=1, prob=probs)
  }
  return(parents)
}


#' @title Generate thread
#' @description tree encoded in a dataframe
gen.thread.Aragon2017 <- function(n=100, alpha=0, beta = 0, tau=1, kappa=0, k=7){

  if(n==1) { return(1) }

  likelihood <- 0
  # n-1 parents because root has no parent
  parents <- c()
  users <- c()
  parents_users <- c()
  likelihoods <- c()

  # Second post (1st after root) has no choice, always choses the root
  parents[1] <- 1
  users[1] <- 2
  parents_users[1] <- 1
  likelihoods[1] <- 1

  # Start from the 3rd post (2nd after root), which arrives at t=2
  for (t in 2:(n-1)){
    betas <- c(exp(beta), rep(0, t-1))
    # Note that the latest post has lag = tau
    lags <- t:1
    # We consider an undirected graph, and every existing node has degree equal to one initially
    popularities <- 1 + tabulate(parents[1:t-1], nbins=t)
    # but root has no outcoming link
    popularities[1] <- popularities[1] -1

    #AUTHOR MODEL
    p_new <- runif(1) < (t)^(-1/k) # probability of new_user: t^(-1/k)
    if (p_new) {
      # choosing a new node
      users[t] <- max(users)+1
    }
    else {
      # choosing existing user according to how many times she has been replied in the thread
      parents_users_prob = 2^tabulate(users)
      # the author model does not allow two consecutive comments to be written by the same user.
      parents_users_prob[users[t-1]] <- 0
      parents_users_prob[is.na(parents_users_prob)] <- 0
      users[t] <- sample(1:length(parents_users_prob), size=1, prob=parents_users_prob/sum(parents_users_prob))
    }
    reciprocities <- c()
    for (r in 1:t-1) {
      reciprocities[r] <- ifelse(users[t]==parents_users[r],1,0)
    }
    reciprocities <- c(0, reciprocities)

    # Attractiveness function
    probs <- alpha * popularities + betas + tau^lags + exp(kappa) * reciprocities

    # A user cannot self-reply a comment made by herself
    if ( users[t] == 1) { probs[1] <- 0 }
    for (i in 2:length(probs)) {
      if ( users[t] == users[i-1]) { probs[i] <- 0 }
    }

    # A user can only reply once to a same comment
    user_matches = which(users[1:t-1] %in% users[t])
    for (i in user_matches) { probs[parents[i]] <- 0}

    # Add new vertex attached to the chosen node
    if(sum(probs) == 0) probs = rep(1/t, t)
    else probs <- probs/sum(probs)

    parents[t] <- sample(length(probs), size=1, prob=probs)
    if ((parents[t]-1)>0){ parents_users[t] = users[parents[t]-1] }
    else { parents_users[t] = 1 }
    likelihoods[t] = probs[parents[t]]

  }

  thread <- parents_to_dataframe(parents)
  thread$thread <- stri_rand_strings(1, 10)
  thread$date <- thread$post
  thread$user = users
  thread$parent = parents
  thread$parent.user = parents_users
  grandparents <- sapply(seq_along(thread$parent), function(t){
    if(thread$parent[t]==1) {
      # reply to root have no grandparent
      FALSE
    }
    else{
      if(thread$user[t]==thread$user[which(thread$post == thread$parent[t])]){
        #if the user is replying to herself, grandparent = F
        FALSE
      }else{
        #original
        thread$user[t]==thread$parent.user[which(thread$post == thread$parent[t])]
      }
    }
  })
  thread$grandparent <- ifelse(grandparents,1,0)
  thread$likelihood <- likelihoods
  return(thread)
}


#' Generates a tree given its posts authors
#' and the estimated parameters of the model (Lumbreras 2016)
#' @param params estimated model parameters
#' @param users sequence of users (equals to tree size)
#' @description Assumes that the identitity and cluster of users are known
gen.thread.Lumbreras2016 <- function(users, alphas, betas, taus){

  # add a fake root so that the index is easier to understand
  # i=1 -> root
  # i=2 -> first reply, etc
  #users <- c(NA, users) # removed since we include root in the users
  alphas <- c(NA, alphas)
  betas <- c(NA, betas)
  taus <- c(NA, taus)

  n <- length(users)
  if(n < 3) stop('Thread is too short')

  # First post has no choice
  g <- graph.empty(n=2)
  g <- add_edges(g, c(2,1))

  popularities <- rep(1,n)
  popularities[1] <- 2 # root has the initial one plus the first reply

  for (i in 3:n){

    # Get post parameters
    bs <- c(betas[i], rep(0, i-2))
    lags <- (i-1):1

    #popularities <- 1 + degree(g, mode="in") # even root starts with degree 1

    # Probability of choosing every node (only one is chosen)
    probs <- alphas[i]*popularities[1:(i-1)] + bs + taus[i]^lags
    probs <- probs/sum(probs)

    j <- sample(1:length(probs), 1, prob=probs)

    # update metrics t avoid using igraph (slower) for that
    popularities[j] <- popularities[j] + 1

    # Add new vertex attached to the chosen node
    g <- add_vertices(g, 1)
    g <- add_edges(g, c(i,j))
  }
  g
}


# Create dataframe from a set of trees
#####################################
if(FALSE){
  # Prepare all the information needed into a nice dataframe
  library(parallel)
  ncores <- detectCores() - 2
  cl<-makeCluster(ncores, outfile="", port=11439)
  clusterEvalQ(cl, {library(igraph); library(dplyr)})
  data.parlapply <- parLapply(cl, trees, tree.to.data)
  stopCluster(cl)

  # join results adding thread id
  for (i in 1:length(data.parlapply)){
    data.parlapply[[i]]$thread <- i
  }
  data.parlapply <- rbindlist(data.parlapply)
  df.trees <- data.frame(thread = data.parlapply$thread,
                         user = data.parlapply$user,
                         post = data.parlapply$post,
                         t = data.parlapply$t,
                         parent = data.parlapply$parent,
                         popularity = data.parlapply$popularity,
                         lag = data.parlapply$lag)

  save(df.trees, file='df.trees.Rda')

}
