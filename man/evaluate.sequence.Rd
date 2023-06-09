% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/evaluate.sequence.R
\name{evaluate.sequence}
\alias{evaluate.sequence}
\title{Evaluate sequence}
\usage{
evaluate.sequence(tree.list, val, exploration.dat, lambdas)
}
\arguments{
\item{tree.list:}{list of trees returned by create.sequence function}

\item{val:}{validation dataframe with outcome Y, exposure variable treat}

\item{exploration.dat:}{exploration dataframe with outcome Y, exposure
variable treat}

\item{lambdas:}{regularization values to use}
}
\value{
dataframe with one row for each tree in sequence and lambda value,
representing the ability of the tree to explain the effect heterogeneity
}
\description{
Evaluate list of trees returned by create.sequence function
}
