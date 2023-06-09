#' @title
#' TBD
#'
#' @description
#' Calculates the sequence of candidate trees.
#'
#'
#' @param exploration.dat: exploration dataframe with outcome Y and exposure
#' variable treat
#' @param ulist.used: user-specified functions to supply to rpart
#' (return value of define_u.list)
#' @param parms.used: additional parameters to supply to rpart
#'
#'
#' @returns
#' list of nested decision trees and corresponding goodness metrics
#'
<<<<<<< HEAD
create.sequence <- function(exploration.dat, ulist.used, parms.used) {
  exploration.dat <- exploration.dat %>% tibble::rowid_to_column("dummy")
=======
create.sequence = function(exploration.dat, ulist.used, parms.used){

  exploration.dat <- exploration.dat %>% tibble::rowid_to_column('dummy')
>>>>>>> na/main

  # Fit a large tree using the user written splitting functions
  a <- rpart::rpart(
    dummy ~ em1 + em2 + em3 + em4,
    # f,
    data = exploration.dat,
<<<<<<< HEAD
    method = ulist.used,
    parms = parms.used,
    control = rpart.control(cp = -10, minbucket = 1, maxsurrogate = 0, maxcompete = 0)
  )

  if (dim(a$frame)[1] == 1) { # Deal with the root only tree

    tree.list <- list(a)
    g.h.list <- list(Inf)
    return(list(tree.list = tree.list, g.h.list = g.h.list))
  } else {
=======
    method   = ulist.used,
    parms    = parms.used,
    control  = rpart.control(cp = -10, minbucket = 1, maxsurrogate = 0, maxcompete = 0))

  if (dim(a$frame)[1] == 1){ # Deal with the root only tree

    tree.list = list(a)
    g.h.list = list(Inf)
    return(list(tree.list = tree.list, g.h.list = g.h.list))

  } else {

>>>>>>> na/main
    # Finding which variables are leaf nodes
    is.leaf <- (a$frame$var == "<leaf>")

    # A function adapted from the partykit package that identifies the rows of the frame
    # which correspond to child nodes of row i in frame matrix
    rpart.kids <- function(i, is.leaf) {
<<<<<<< HEAD
      if (is.leaf[i]) {
        return(NULL)
      } else {
        return(c(
          i + 1L,
          which((cumsum(!is.leaf[-(1L:i)]) + 1L) == cumsum(is.leaf[-(1L:i)]))[1L] + 1L + i
        ))
      }
    }

    # Finding goodness of the split
    a$frame$split.stat <- 0
    a$frame$split.stat[!is.leaf] <- a$splits[, 3]

    # Calculating the g(h) parameter for each non-terminal node
    g.h <- rep(0, nrow(a$frame))
    for (i in 1:nrow(a$frame)) {
      if (is.leaf[i]) {
        g.h[i] <- Inf
      } else {
        # Find all kids of node i
        kids.i <- i
        stop.loop <- FALSE
        while (stop.loop == FALSE) {
          kids.old <- kids.i
          for (j in 1:length(kids.i)) {
            kids.i <- unique(c(kids.i, rpart.kids(kids.i[j], is.leaf)))
          }
          if (length(kids.i) == length(kids.old)) {
            stop.loop <- TRUE
          }
          # Calculating g.h for node i
          g.h[i] <- sum(a$frame$split.stat[kids.i]) / sum(a$frame$split.stat[kids.i] != 0)
=======
      if (is.leaf[i]) return(NULL)
      else return(c(i + 1L,
                    which((cumsum(!is.leaf[-(1L:i)]) + 1L) == cumsum(is.leaf[-(1L:i)]))[1L] + 1L + i))
    }

    # Finding goodness of the split
    a$frame$split.stat = 0
    a$frame$split.stat[!is.leaf] = a$splits[, 3]

    # Calculating the g(h) parameter for each non-terminal node
    g.h = rep(0, nrow(a$frame))
    for(i in 1:nrow(a$frame)){
      if(is.leaf[i]){g.h[i] = Inf} else{
        # Find all kids of node i
        kids.i = i
        stop.loop = FALSE
        while(stop.loop == FALSE){
          kids.old = kids.i
          for(j in 1:length(kids.i)){
            kids.i = unique(c(kids.i, rpart.kids(kids.i[j], is.leaf)))
          }
          if(length(kids.i) == length(kids.old)){stop.loop = TRUE}
          # Calculating g.h for node i
          g.h[i] = sum(a$frame$split.stat[kids.i])/sum(a$frame$split.stat[kids.i] != 0)
>>>>>>> na/main
        }
      }
    }

    # Adding g.h to frame
<<<<<<< HEAD
    a$frame$g.h <- g.h

    # Start pruning
    # First tree is the large tree
    tree.list <- list(a)
    g.h.list <- list(0)
    # pruned.node = list(NULL)
    stop.prune <- FALSE
    k <- 1

    while (stop.prune == FALSE) {
      tree.used <- tree.list[[k]]
      # Calculating the g(h) parameter for each non-terminal node
      tree.used$frame$g.h <- rep(0, nrow(tree.used$frame))
      is.leaf.prune <- (tree.used$frame$var == "<leaf>")
      # Setting splitting statistics for new terminal nodes to 0
      tree.used$frame$split.stat[is.leaf.prune] <- 0

      # Calculating the g(h) function for each non-terminal node
      for (i in 1:nrow(tree.used$frame)) {
        if (is.leaf.prune[i]) {
          tree.used$frame$g.h[i] <- Inf
        } else {
          # Find all kids of node i
          kids.i <- i
          stop.loop <- FALSE
          while (stop.loop == FALSE) {
            kids.old <- kids.i
            for (j in 1:length(kids.i)) {
              kids.i <- unique(c(kids.i, rpart.kids(kids.i[j], is.leaf.prune)))
            }
            if (length(kids.i) == length(kids.old)) {
              stop.loop <- TRUE
            }
            tree.used$frame$g.h[i] <- sum(tree.used$frame$split.stat[kids.i]) / sum(tree.used$frame$split.stat[kids.i] != 0)
=======
    a$frame$g.h = g.h

    # Start pruning
    # First tree is the large tree
    tree.list = list(a)
    g.h.list = list(0)
    # pruned.node = list(NULL)
    stop.prune = FALSE
    k = 1

    while(stop.prune == FALSE){
      tree.used = tree.list[[k]]
      # Calculating the g(h) parameter for each non-terminal node
      tree.used$frame$g.h = rep(0, nrow(tree.used$frame))
      is.leaf.prune <- (tree.used$frame$var == "<leaf>")
      # Setting splitting statistics for new terminal nodes to 0
      tree.used$frame$split.stat[is.leaf.prune] = 0

      # Calculating the g(h) function for each non-terminal node
      for(i in 1:nrow(tree.used$frame)){
        if(is.leaf.prune[i]){tree.used$frame$g.h[i] = Inf} else{
          # Find all kids of node i
          kids.i = i
          stop.loop = FALSE
          while(stop.loop == FALSE){
            kids.old = kids.i
            for(j in 1:length(kids.i)){
              kids.i = unique(c(kids.i, rpart.kids(kids.i[j], is.leaf.prune)))
            }
            if(length(kids.i) == length(kids.old)){stop.loop = TRUE}
            tree.used$frame$g.h[i] = sum(tree.used$frame$split.stat[kids.i])/sum(tree.used$frame$split.stat[kids.i] != 0)
>>>>>>> na/main
          }
        }
      }

      # Finding the value which minimizes g(h) (among internal nodes)
<<<<<<< HEAD
      to.prune <- which.min(tree.used$frame$g.h)
      # Finding the minimum g.h value
      g.h.min <- min(tree.used$frame$g.h)

      # Find all kids of node to.prune
      kids.i <- to.prune
      stop.loop <- FALSE
      while (stop.loop == FALSE) {
        kids.old <- kids.i
        for (j in 1:length(kids.i)) {
          kids.i <- unique(c(kids.i, rpart.kids(kids.i[j], is.leaf.prune)))
        }
        if (length(kids.i) == length(kids.old)) {
          stop.loop <- TRUE
        }
=======
      to.prune = which.min(tree.used$frame$g.h)
      #print(tree.used$frame[to.prune,])
      # Finding the minimum g.h value
      g.h.min = min(tree.used$frame$g.h)

      # Find all kids of node to.prune
      kids.i = to.prune
      stop.loop = FALSE
      while(stop.loop == FALSE){
        kids.old = kids.i
        for(j in 1:length(kids.i)){
          kids.i = unique(c(kids.i, rpart.kids(kids.i[j], is.leaf.prune)))
        }
        if(length(kids.i) == length(kids.old)){stop.loop = TRUE}
>>>>>>> na/main
      }


      # Finding number of splits to prune
<<<<<<< HEAD
      split.to.prune <- length(kids.i[which(!is.leaf.prune[kids.i])])

      # Creating the new splits and frames for new tree
      splits.new <- tree.used$splits[-c(sum(!is.leaf.prune[1:to.prune]):(sum(!is.leaf.prune[1:to.prune]) + split.to.prune - 1)), ]
      frame.new <- tree.used$frame[-setdiff(kids.i, to.prune), ]

      # Changing all nodes that were internal nodes and are now terminal node to terminal nodes
      frame.new$var[to.prune] <- "<leaf>"

      tree.new <- tree.used
      tree.new$frame <- frame.new
      
      if ("matrix" %in% class(splits.new)) {
        tree.new$splits <- splits.new
      }
      if ("numeric" %in% class(splits.new)) {
        tree.new$splits <- matrix(splits.new, nrow = 1)
        colnames(tree.new$splits) <- colnames(tree.used$splits)
      }

      # Changing the terminal node for $where in rpart object
      tree.new$where <- tree.used$where
      tree.new$where[tree.new$where %in% kids.i] <- to.prune
      tree.new$where[tree.new$where > max(kids.i)] <- tree.new$where[tree.new$where > max(kids.i)] - length(kids.i) + 1
      tree.new$where <- as.integer(tree.new$where)

      k <- k + 1
=======
      split.to.prune = length(kids.i[which(!is.leaf.prune[kids.i])])

      # Creating the new splits and frames for new tree
      splits.new = tree.used$splits[-c(sum(!is.leaf.prune[1:to.prune]):(sum(!is.leaf.prune[1:to.prune]) + split.to.prune - 1)), ]
      frame.new = tree.used$frame[-setdiff(kids.i, to.prune), ]

      # Changing all nodes that were internal nodes and are now terminal node to terminal nodes
      frame.new$var[to.prune] =  "<leaf>"

      tree.new = tree.used
      tree.new$frame = frame.new
      print(class(splits.new))
      if("matrix" %in% class(splits.new)){tree.new$splits = splits.new}
      if("numeric" %in% class(splits.new)){
        tree.new$splits = matrix(splits.new, nrow = 1)
        colnames(tree.new$splits) = colnames(tree.used$splits)
      }

      # Changing the terminal node for $where in rpart object
      tree.new$where = tree.used$where
      tree.new$where[tree.new$where %in% kids.i] = to.prune
      tree.new$where[tree.new$where > max(kids.i)] = tree.new$where[tree.new$where > max(kids.i)] - length(kids.i) + 1
      tree.new$where = as.integer(tree.new$where)

      k = k+1
>>>>>>> na/main
      # Add tree and lambda to the list
      tree.list[[k]] <- tree.new
      g.h.list[[k]] <- g.h.min
      # pruned.node[[k]] <-

<<<<<<< HEAD
      if (sum(tree.new$frame$var == "<leaf>") == 1) {
        stop.prune <- TRUE
      }
=======
      if(sum(tree.new$frame$var == "<leaf>") == 1){stop.prune = TRUE}
>>>>>>> na/main
    }
    return(list(tree.list = tree.list, g.h.list = g.h.list))
  }
}
