#' Summed Table
#'
#' This function can be used to calculate the sum of the tables for each combination of raters. The output table can be used to estimate the agreement statistics between two or more raters. First the crosstable is made for each combination of raters. This results in \code{2*(m-1)} tables, where \code{m} is the number of raters. Then these tables are summed to one table.
#'
#' @param df The input data frame that contains the scores for each rater in each column
#' @param ratings A character vector that contains the names of the factor variables that need to be used as ratings
#' @param levels A character vector that contains the levels of the factors.
#' @param offdiag A logical parameter indicating if the of diagonal means should be used, default is TRUE with more than two raters
#'
#' @return Returns a contingency table, an object of class "table", an array of integer values.
#' @export
#' @importFrom utils combn
#' @importFrom dplyr %>%
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
sumtable <- function(df, ratings=NULL, levels=NULL, offdiag=NULL){
  ## input to data.frame
  #
  if(!is.data.frame(df)){warning("Data are automatically transformed to a data.frame.")}
  df <- as.data.frame(df)

  ## ratings are colnames
  if(is.null(ratings)){
    ratings=colnames(df)}
  stopifnot(is.character(ratings))

  ## variables to factors with equalnr of levels
  if(!all(purrr::map_chr(df[ratings], class)=="factor")){
    warning("Variables are automatically transformed to factors.")
    }

  if(is.null(levels)){
    levels = df %>% purrr::map(unique) %>% unlist() %>% unique %>% sort
  }

  if(is.null(offdiag)){
    if(length(levels)==2){offdiag=FALSE}
    if(length(levels)>3){offdiag=TRUE}
    }

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
  if(offdiag==TRUE){
  ## off diagonal means in matrix
  mat1 <-sumtable*0
  for (i in 1:length(levels)){
    for (j in 1:length(levels)){
    mat1[i,j] <- (sumtable[i,j]+sumtable[j,i])/2
    mat1[j,i] <- (sumtable[i,j]+sumtable[j,i])/2
    }
    }
  sumtable <- mat1
  }
  sumtable
  }


