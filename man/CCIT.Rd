% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/CCIT.R
\name{CCIT}
\alias{CCIT}
\title{Estimate conditional average treatment effect}
\usage{
CCIT(
  matched.exploration.sample.outcomes,
  matched.validation.sample.outcomes,
  matched.inference.sample.outcomes,
  lambdas,
  stopping.rule
)
}
\arguments{
\item{matched.exploration.sample.outcomes:}{The exploration dataframe with
outcome Y, exposure variable treat.}

\item{matched.validation.sample.outcomes:}{The validation dataframe
with outcome Y, exposure variable treat.}

\item{matched.inference.sample.outcomes:}{The inference dataframe
with outcome Y, exposure variable treat.}

\item{lambdas:}{A vector of values to use as the regularization parameter in
the CCIT algorithm.}

\item{stopping.rule:}{A boolean value to indicate whether the tree-splitting
algorithm should stop when the estimated interaction effect is <1/10 of
the overall effect.}
}
\value{
list of:
- est.treatment.effects: For each lambda value, dataframe with CATE
estimates for all observations
- selected.trees: For each lambda value, selected decision tree
- tree.list: all decisions trees in the sequence
- selected.tree.size: For each lambda value, size of selected decision
tree
}
\description{
Performs CCIT algorithm to estimate conditional average treatment effect.
}
