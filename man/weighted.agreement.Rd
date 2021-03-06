% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/agreements.R
\name{weighted.agreement}
\alias{weighted.agreement}
\title{Weighted agreement}
\usage{
weighted.agreement(data, weight = 1, ...)
}
\arguments{
\item{data}{A data matrix or table with equal number of columns and rows. Or a data frame that contains the scores for each rater in each column.}

\item{weight}{weight for the one-off category, defaul \code{weight = 1}}

\item{\dots}{options for sumtable}
}
\value{
An S3 object containing the proportion of agreement.
}
\description{
The agreement between 2 or more raters when they may be one category off, that category is weighted by 1 by default. Only relevant for ordinal rating scales with more than 2 Likert categories.
}
\examples{
df <- data.frame(r1=factor(c(1,2,2,0,3,3,1,0,3,0,2,2,0,3,1)),
                 r2=factor(c(1,1,1,0,3,3,1,0,1,0,2,2,0,2,1)),
                 r3=factor(c(1,1,1,3,3,2,1,0,1,0,2,2,0,3,1)),
                 r4=factor(c(1,2,1,0,3,3,1,0,3,0,2,2,0,2,1)))
table <- sumtable(df=df, ratings=c("r1", "r2", "r3", "r4"), levels=c("0","1", "2", "3"))
weighted.agreement(table)
}
