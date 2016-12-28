#' Summed Table
#'
#' This function can be used to calculate the sum of the tables for each combination of raters. The output table can be used to estimate the agreement statistics between two or more raters. First the crosstable is made for each combination of raters. This results in \code{2*(m-1)} tables, where \code{m} is the number of raters. Then these tables are summed to one table.
#'
#' @param df The input data frame that contains the scores for each rater in each column
#' @param ratings A character vector that contains the names of the factor variables that need to be used as ratings
#' @param levels A character vector that contains the levels of the factors
#'
#' @return Returns a contingency table, an object of class "table", an array of integer values.
#' @export
#'
#' @examples
#' df <- data.frame(r1=factor(c(1,0,1,0,0,1,1,0,0,0,1,1,0,1,1)),
#'                  r2=factor(c(1,1,1,1,0,1,1,0,0,0,1,1,0,1,0)),
#'                  r3=factor(c(1,1,1,0,0,0,1,1,1,0,0,1,0,1,1)),
#'                  r4=factor(c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1)))
#' sumtable(df=df, ratings=c("r1", "r2", "r3", "r4"), levels=c("0","1"))
#'
#' df <- data.frame(r1=factor(c(1,2,2,0,3,3,1,0,3,0,2,2,0,3,1)),
#'                  r2=factor(c(1,1,1,0,3,3,1,0,1,0,2,2,0,2,1)),
#'                  r3=factor(c(1,1,1,3,3,2,1,0,1,0,2,2,0,3,1)),
#'                  r4=factor(c(1,2,1,0,3,3,1,0,3,0,2,2,0,2,1)))
#' sumtable(df=df, ratings=c("r1", "r2", "r3", "r4"), levels=c("0","1", "2", "3"))
sumtable <- function(df, ratings, levels){
 stopifnot(is.data.frame(df))
  stopifnot(is.character(ratings))
  stopifnot(all(purrr::map_chr(df[ratings], class)=="factor"))

    df <- df[ratings]
  nraters <- length(ratings)
  crosval <- t(combn(ratings,2))

  for (z in 1:nrow(crosval)) {
    v1 <- df[crosval[z,1]][,1]
    v1.1 <- factor(v1, levels = levels)
    v2 <- df[crosval[z,2]][,1]
    v2.1 <- factor(v2, levels = levels)
    table  <- table(v1.1, v2.1, dnn=NULL)
    crostab <- table
    if (z == 1) {
      sumtable <- crostab
    }
    if (z > 1) {
      sumtable <- sumtable + crostab
    }
 }
  sumtable
}

