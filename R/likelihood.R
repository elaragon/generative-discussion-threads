library(dplyr)
library(data.table)
library(igraph)
library(parallel)

#' Total likelihood of a dataframe according to Gomez et al. 2013
#' @param data data.frame with one post per row and features in columns.
#' @param list of model parameters
#' @return loglikelihood of the dataset
#' @details df.tree must not have any non-numerical value since the internal apply
#' won't know how to deal with that
#' @export
# x100 times faster (for large dataframes)
likelihood_Gomez2013 <- function(data, params){
  alpha <- params$alpha
  beta <- params$beta
  tau <- params$tau
  sum(log(alpha*data['popularity'] + beta*data['root'] + tau^data['lag']))-
    sum(log(2*alpha*(data['t']-1) + beta + tau*(tau^data['t']-1)/(tau-1)))
}

#' Total likelihood of a dataframe according to Aragón et al. 2017
#' @param data data.frame with one post per row and features in columns.
#' @param list of model parameters
#' @return loglikelihood of the dataset
#' @details df.tree must not have any non-numerical value since the internal apply
#' won't know how to deal with that
#' @export
likelihood_Aragon2017 <- function(data, params){
  alpha <- params$alpha
  beta <- params$beta
  tau <- params$tau
  kappa <- params$kappa
  threads = unique(data$thread)
  threads_likelihoods = c()
  for (th in threads) {
    thread <- data %>% filter(thread == th)
    likelihoods = likelihoods_thread_Aragon2017(thread, params)
    threads_likelihoods[match(th,threads)] = sum(log(likelihoods))
  }
  sum(threads_likelihoods)
}


likelihoods_thread_Aragon2017 <- function(thread, params){
  alpha <- params$alpha
  beta <- params$beta
  tau <- params$tau
  kappa <- params$kappa

  likelihood <- 1
  likelihoods = c(1)

  if(nrow(thread)==1) { likelihood = likelihood + 1 }
  else {
    max_t = length(thread$t)
    for (t in 2:max_t) {
      users = thread$user[1:t]
      parents_users = thread$parent.user[1:t-1]
      parents = thread$parent[1:t]

      betas <- c(exp(beta), rep(0, t-1))
      # Note that the latest post has lag = tau
      lags <- t:1
      # We consider an undirected graph, and every existing node has degree equal to one initially
      popularities <- 1 + tabulate(parents[1:t-1], nbins=t)
      # but root has no outcoming link
      popularities[1] <- popularities[1] -1

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

      likelihoods[t] <- probs[thread$parent[t]]

    }
  }
  likelihoods
}
