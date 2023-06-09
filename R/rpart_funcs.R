#' @title
#' TBD
#'
#' @description
#' Defines functions for custom rpart function
#'
#' required libraries: rpart, rpart.plot
#'
#' @param stopping.rule: A boolean to indicate whether the tree-splitting
#' algorithm should stop when the estimated interaction effect is <1/10 of
#' the overall effect.
#'
define_u.list <- function(stopping.rule = TRUE) {
<<<<<<< HEAD
  criterion <- function(Y, treat) {
    lmod <- lm(Y ~ treat + 1)
    lmod$coefficients["treat"]
  }
  # initialization function for rpart
  itemp <- function(y, offset, parms, wt) {
    sfun <- function(yval, dev, wt, ylevel, digits) {
      paste(" mean=", format(signif(yval, digits)),
        ", MSE=", format(signif(dev / wt, digits)),
        sep = ""
      )
=======
  criterion <- function(Y,treat) {
    lmod <- lm(Y ~ treat + 1)
    lmod$coefficients['treat']
  }
  # initialization function for rpart
  itemp <- function(y, offset, parms, wt) {

    sfun <- function(yval, dev, wt, ylevel, digits ) {
      paste(" mean=", format(signif(yval, digits)),
            ", MSE=" , format(signif(dev/wt, digits)),
            sep = '')
>>>>>>> na/main
    }

    environment(sfun) <- .GlobalEnv
    list(y = c(y), parms = parms, numresp = 1, numy = 1, summary = sfun)
  }

  # evaluation function for rpart - I don't think I can compute the complexity measure as defined by Su without x. I could pass as a parm
  etemp <- function(y, wt, parms) {
<<<<<<< HEAD
    Y <- parms$Y[y]
    treat <- parms$treat[y]
    list(label = criterion(Y, treat), deviance = sd(Y))
  }

  # splitting function for rpart
  stemp <- function(y, wt, x, parms, continuous) {
=======
    Y = parms$Y[y]
    treat = parms$treat[y]
    list(label = criterion(Y,treat), deviance = sd(Y))
  }

  # splitting function for rpart
  stemp <- function(y, wt, x, parms, continuous){
>>>>>>> na/main
    # only 1 possible split per covariate
    Y <- parms$Y[y]
    treat <- parms$treat[y]
    EM <- x
    ux <- sort(unique(x))
<<<<<<< HEAD
    means <- data.frame(Y = Y, EM = EM) %>%
      group_by(EM) %>%
      summarise_at(vars(Y), mean) %>%
      .$Y
    ord <- order(means)
    lmod <- lm(Y ~ EM * treat)
=======
    means <- data.frame(Y = Y, EM = EM) %>% group_by(EM) %>% summarise_at(vars(Y), mean) %>% .$Y
    ord <- order(means)
    lmod <- lm(Y ~ EM*treat)
>>>>>>> na/main
    t <- abs(coef(summary(lmod))["EM:treat", "t value"])
    df <- df.residual(lmod)
    inter <- abs(coef(summary(lmod))["EM:treat", "Estimate"])
    # if there is only a single observation in any EM category, set goodness to 0
    if (min(table(EM)) <= 1) {
<<<<<<< HEAD
      goodness <- c(0)
    } else if (stopping.rule && inter <= parms$overall_effect / 10) {
      goodness <- c(0)
=======
      goodness = c(0)
    }
    else if (stopping.rule && inter <= parms$overall_effect/10 ) {
      goodness = c(0)
>>>>>>> na/main
    }
    # if the estimated interaction effect is less than 1/10 of the overall effect, set goodness to 0
    # else if (t <= 500) {
    # else if (inter <= parms$overall_effect/10) {
    #   goodness = c(0)
    # }
    # otherwise, set goodness to the t-statistic for the interaction term
    else {
<<<<<<< HEAD
      goodness <- c(t**2)
      # goodness = c(p)
=======
      goodness = c(t**2)
      #goodness = c(p)
>>>>>>> na/main
    }
    list(goodness = goodness, direction = ux[ord])
  }

  ulist.used <- list(eval = etemp, split = stemp, init = itemp)
  return(ulist.used)
}
