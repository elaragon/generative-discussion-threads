#'@title Degree distribution
#'@param parents list of parent vectors
struct_degree_distribution <-function(parents){
  # Create a dataframe with node-degree
  degrees <- c()
  for(pi in parents){
    degrees <- c(degrees, tabulate(pi, nbins=length(pi)))
  }
  
  # Compute the frequency of each degree. Include freq. of degree 0
  degrees.name <- 0:max(degrees) 
  frequencies <- tabulate(degrees+1)
  
  # Create a dataframe degree-frequency
  df.degrees <- data.frame(degree = degrees.name, frequency = frequencies)
  return(df.degrees)
}


#'@title Distribution of subtrees size
#'@description The subtree size for node i is the size of the tree 
#'of the descendants of i 
#'@value A vector with all the subtree sizes
struct_subtree_size_distribution <-function(parents){
  sizes.all <- c()
  ntrees <- length(parents)
  for(i in 1:ntrees){
    if (!(i %% 100000)) { cat("\nProcessed ", i, "/", ntrees) }
    pi <- parents[[i]]
    gtree <- parents_to_tree(pi)
    sizes <- ego_size(gtree, order=1000, mode='in', mindist=1)
    sizes.all <- c(sizes.all, sizes)
  }
  
  # Compute the frequency of each size Include freq. of size 0
  sizes.name <- 0:max(sizes.all) 
  frequencies <- tabulate(sizes.all+1)
  
  # Create a dataframe degree-frequency
  df.degrees <- data.frame(size = sizes.name, frequency = frequencies)
  return(df.degrees)
}


#'@title  Depth vs Size
#'@params A list of parent vectors
#'@value  Datafrem with size and depth of each tree
struct_size_depth <- function(parents){
  ntrees <- length(parents)
  df <- c()
  for(i in 1:ntrees){
    if (!(i %% 100000)) { cat("\nProcessed ", i, "/", ntrees) }
    pi <- parents[[i]]
    gtree <- parents_to_tree(pi)
    depth <- diameter(gtree)
    size  <- length(pi)
    df <- rbind(df, c(size, depth))
  }
  df <- as.data.frame(df)
  names(df) <- c('size', 'depth')
  return(df)
}


