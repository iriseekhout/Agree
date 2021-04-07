#' Weighted agreement
#'
#' The agreement between 2 or more raters when they may be one category off, that category is weighted by 1 by default. Only relevant for ordinal rating scales with more than 2 Likert categories.
#'
#' @param data A data matrix or table with equal number of columns and rows. Or a data frame that contains the scores for each rater in each column.
#' @param weight weight for the one-off category, default \code{weight = 1}
#' @param \dots options for sumtable
#' @return An S3 object containing the proportion of agreement.
#' @export
#'
#' @examples
#' df <- data.frame(r1=factor(c(1,2,2,0,3,3,1,0,3,0,2,2,0,3,1)),
#'                  r2=factor(c(1,1,1,0,3,3,1,0,1,0,2,2,0,2,1)),
#'                  r3=factor(c(1,1,1,3,3,2,1,0,1,0,2,2,0,3,1)),
#'                  r4=factor(c(1,2,1,0,3,3,1,0,3,0,2,2,0,2,1)))
#' table <- sumtable(df=df, ratings=c("r1", "r2", "r3", "r4"), levels=c("0","1", "2", "3"))
#' weighted.agreement(table)
weighted.agreement <- function(data, weight=1, ...){
  if(is.data.frame(data)){
    table <- Agree::sumtable(data, ...)
  }
  if(nrow(data)==ncol(data)){
    table <- data
  }
  stopifnot(nrow(table)>2)
  agreeup <- agreelow <- vector()
  for (i in 1:(ncol(table) - 1)) {
    agreeup[i] <- table[i,i + 1]
    agreelow[i] <- table[i + 1,i]
  }
  agreement.plusone <-  (sum(diag(table),agreeup*weight, agreelow*weight) / sum(table))
  agreement.plusone
}

