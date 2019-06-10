#' Creates a dataframe with predictions from each model
#' and real choices
#' params tree dataframe
compare_link_prediction <- function(df.tree, params.Gomez2013, params.Aragon2017){
  # We can compare with barabasi becaus the choice is independent of the alpha

  parents <- df.tree$parent
  parents.users <- df.tree$parent.user
  users <- df.tree$user

  # Results container
  df.preds <- data.frame()

  # skip root and first post
  # Recall: t has already t posts before (because root t=0)
  # Thus, post at t has t posts to choose from.
  for(t in 2:length(parents)){

    # Data common too all models
    b <- rep(0,t)
    lags <- t:1
    popularities <- 1 + tabulate(parents[1:(t-1)], nbins=t) #  we follow Gomez. root also starts with 1
    grandparents <- c(FALSE, (df.tree$parent.user[1:(t-1)]==as.character(users[t]))) # root is always false
    chosen <- parents[t]

    # Gomez2013 model
    #######################
    alpha <- params.Gomez2013$alpha
    beta <- params.Gomez2013$beta
    tau <- params.Gomez2013$tau
    b[1] <- beta
    probs.Gomez2013 <- alpha * popularities + b + tau^lags

    predicted.Gomez2013 <- which.max(probs.Gomez2013)
    like.Gomez2013 <- log(probs.Gomez2013[chosen]) - log(sum(probs.Gomez2013))
    ranking.Gomez2013 <- rank(-probs.Gomez2013)[chosen]

    # Aragon2017 model
    #######################
    # print(grandparents)
    # check grandparents
    grandparents <- c(FALSE, (df.tree$parent.user[1:(t-1)]==as.character(users[t]))) # root is always false
    potential_brother <- df.tree[which(df.tree$user[1:(t-1)] == users[t]),]$parent   ## select the parent of the potential brother to set it to FALSE in the grandparents vector
    grandparents[potential_brother] <- FALSE
    # print(grandparents)

    alpha <- params.Aragon2017$alpha
    beta <- params.Aragon2017$beta
    tau <- params.Aragon2017$tau
    gamma <- params.Aragon2017$gamma
    b[1] <- beta
    probs.Aragon2017 <- alpha * popularities + gamma*grandparents + b + tau^lags

    predicted.Aragon2017 <- which.max(probs.Aragon2017)
    like.Aragon2017 <- log(probs.Aragon2017[chosen]) - log(sum(probs.Aragon2017))
    ranking.Aragon2017 <- rank(-probs.Aragon2017)[chosen] # the rank of an element is its position in the sorted vector.

    #     predicted: the most likely post
    #     like: likelihood of the choosen post (the real observed one)
    #     ranking: position in which the real choice was placed. Best is 1. Worst is current thread size

    df.preds <- rbindlist(list(df.preds,
                               data.frame(predicted.Gomez2013 =  predicted.Gomez2013,
                                          like.Gomez2013 =   like.Gomez2013,
                                          ranking.Gomez2013 = ranking.Gomez2013,
                                          predicted.Aragon2017 =  predicted.Aragon2017,
                                          like.Aragon2017 = like.Aragon2017,
                                          ranking.Aragon2017 = ranking.Aragon2017,
                                          tree.size = t,
                                          # thread= df.tree$thread[t],
                                          chosen = chosen, ## this is the real parent
                                          user = df.tree$user[t] )))

  }
  df.preds
}
