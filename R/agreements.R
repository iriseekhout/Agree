
#' Agreement
#'
#' The proportion of overall agreement between 2 or more raters is calculated by INCLUDE FORMULA.
#'
#' @param data A data matrix or table with equal number of columns and rows. Or a data frame that contains the scores for each rater in each column.
#' @return An S3 object containing the proportion of overall agreement.
#' @export
#'
#' @examples
#' df <- data.frame(r1=factor(c(1,0,1,0,0,1,1,0,0,0,1,1,0,1,1)),
#'                  r2=factor(c(1,1,1,1,0,1,1,0,0,0,1,1,0,1,0)),
#'                  r3=factor(c(1,1,1,0,0,0,1,1,1,0,0,1,0,1,1)),
#'                  r4=factor(c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1)))
#' table <- sumtable(df=df, ratings=c("r1", "r2", "r3", "r4"), levels=c("0","1"))
#' agreement(table)
#'
#' df <- data.frame(r1=factor(c(1,2,2,0,3,3,1,0,3,0,2,2,0,3,1)),
#'                  r2=factor(c(1,1,1,0,3,3,1,0,1,0,2,2,0,2,1)),
#'                  r3=factor(c(1,1,1,3,3,2,1,0,1,0,2,2,0,3,1)),
#'                  r4=factor(c(1,2,1,0,3,3,1,0,3,0,2,2,0,2,1)))
#' table <- sumtable(df=df, ratings=c("r1", "r2", "r3", "r4"), levels=c("0","1", "2", "3"))
#' agreement(table)
agreement <- function(data, ratings=NULL, levels=NULL, offdiag=NULL){
  if(is.data.frame(data)){
    table <- Agree::sumtable(data,ratings=ratings,levels=levels, offdiag = TRUE)
  }
  if(nrow(data)==ncol(data)){
    table <- data
  }
  agreement <- sum(diag(table)/sum(table))
  agreement
}

#' positive agreement
#'
#' A function to obtain the positive or negative agreement between 2 or more raters when categories are dichotomous. INCLUDE FORMULAS.
#'
#' @param data A data matrix or table with equal number of columns and rows. Or a data frame that contains the scores for each rater in each column.
#' @param specific A character vector indicating whether the \code{"positive"} or \code{"negative"} agreements should be obtained.
#'
#' @return An S3 object containing the proportion of positive (or negative) agreement.
#' @export
#'
#' @examples
#' #' df <- data.frame(r1=factor(c(1,0,1,0,0,1,1,0,0,0,1,1,0,1,1)),
#'                  r2=factor(c(1,1,1,1,0,1,1,0,0,0,1,1,0,1,0)),
#'                  r3=factor(c(1,1,1,0,0,0,1,1,1,0,0,1,0,1,1)),
#'                  r4=factor(c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1)))
#' table <- sumtable(df=df, ratings=c("r1", "r2", "r3", "r4"), levels=c("0","1"))
#' positive.agreement(table, specific="positive")
positive.agreement <- function(data, specific="positive", ratings=NULL, levels=NULL, offdiag=NULL){
  if(is.data.frame(data)){
    table <- Agree::sumtable(data,ratings=ratings,levels=levels, offdiag = TRUE)
  }
  if(nrow(data)==ncol(data)){
    table <- data
  }
  if(specific=="positive"){
  specific.agreement <- (2*(table[1,1]))/((2*(table[1,1]))+(table[1,2])+(table[2,1]))
}
if(specific=="negative"){
  specific.agreement <- (2*(table[2,2]))/((2*(table[2,2]))+(table[1,2])+(table[2,1]))
}
specific.agreement
}

#' Weighted agreement
#'
#' The agreement between 2 or more raters when they may be one category off, that category is weighted by 1 by default. Only relevant for ordinal rating scales with more than 2 Likert categories.
#'
#' @param data A data matrix or table with equal number of columns and rows. Or a data frame that contains the scores for each rater in each column.
#' @return An S3 object containing the proportion of agreement.
#' @export
#'
#' @examples
#' #' df <- data.frame(r1=factor(c(1,2,2,0,3,3,1,0,3,0,2,2,0,3,1)),
#'                  r2=factor(c(1,1,1,0,3,3,1,0,1,0,2,2,0,2,1)),
#'                  r3=factor(c(1,1,1,3,3,2,1,0,1,0,2,2,0,3,1)),
#'                  r4=factor(c(1,2,1,0,3,3,1,0,3,0,2,2,0,2,1)))
#' table <- sumtable(df=df, ratings=c("r1", "r2", "r3", "r4"), levels=c("0","1", "2", "3"))
#' agreement.plusone(table)
weighted.agreement <- function(data,weight=1, ratings=NULL, levels=NULL, offdiag=NULL){
  if(is.data.frame(data)){
    table <- Agree::sumtable(data,ratings=ratings,levels=levels, offdiag = TRUE)
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


#' Conditional agreement for more than 2 categories
#'
#' Conditional agreement when there are more than two categories (averages over discordant cells to correct for random rater combinations). INCLUDE FORMULAS.
#'
#' @param data A data matrix or table with equal number of columns and rows.
#' @return conditionaltable is a table with conditional agreement proportions. On the diagonal the specific agreement proportions for each category are displayed.
#' @export
#'
#' @examples
#' #' df <- data.frame(r1=factor(c(1,2,2,0,3,3,1,0,3,0,2,2,0,3,1)),
#'                  r2=factor(c(1,1,1,0,3,3,1,0,1,0,2,2,0,2,1)),
#'                  r3=factor(c(1,1,1,3,3,2,1,0,1,0,2,2,0,3,1)),
#'                  r4=factor(c(1,2,1,0,3,3,1,0,3,0,2,2,0,2,1)))
#' conditional.agreement(df)
conditional.agreement <- function(data, ratings=NULL, levels=NULL, offdiag=NULL){
  if(is.data.frame(data)){
    table <- Agree::sumtable(data,ratings=ratings,levels=levels, offdiag = TRUE)
  }
  if(nrow(data)==ncol(data)){
    table <- data
  }
  mat1 <- table*0
  if(is.null(colnames(table))){levels <- 1:nrow(table)}
  else {levels <- colnames(table)}
  for (i in seq_along(levels)){
    for (j in seq_along(levels)) {
      mat1[i,j] <- (table[i,j] + table[j,i]) / 2
      mat1[j,i] <- (table[i,j] + table[j,i]) / 2
    }
  }
  mat1_rowtot <- rowSums(mat1)
  mat1.2 <- cbind(mat1,mat1_rowtot)
  mat1.2_coltot <- colSums(mat1.2)
  mat1.2_colprop <- (mat1.2_coltot / mat1.2_coltot[length(levels) + 1])
  betweentable <- rbind(mat1.2, c(mat1.2_colprop[1:length(levels) ],mat1.2_coltot[length(levels) + 1]))
  rownames(betweentable) <- c(paste(levels), "PROP")
  colnames(betweentable) <- c(paste(levels), "SUM")
  betweentable <-round(betweentable,3)
  cat.proportions <- mat1.2_colprop[1:length(levels)]
  mat2 <- matrix (0,length(levels) ,length(levels) )
  for (i in seq_along(levels)) {
    mat2[i,] <- mat1[i,] / mat1_rowtot[i]
  }
  prop_agree_cat <- round(mat2, 3)
  rownames(prop_agree_cat) <- c(paste(levels))
  colnames(prop_agree_cat) <- c(paste(levels))
  specific.agreement <- diag(prop_agree_cat)
  cel.proportions <- prop_agree_cat
  conditionaltable <- cbind(prevalence=rowSums(table),proportion=rowSums(table)/sum(rowSums(table)), cel.proportions)
  conditionaltable
}


#' Specific agreement
#'
#' specific agreement (averages over discordant cells to correct for random rater combinations). INCLUDE FORMULAS. When there are 2 categories, this is equal to the postive/negative agreement. When there are more than two categories, one can either look at the agreement for one category versus the others, for example very satisfied verus rest or for one category versus one specific other categorie, for example very satisfies versus not satisfied.
#'
#' @param data A data matrix or table with equal number of columns and rows. Or a data frame that contains the scores for each rater in each column.
#' @param cat1 A character indicating the category for which specific agreement should be obtained.
#' @param cat2 A character indicating the category to which the specific agreement should be compared, if left empty all other categories are used.
#'
#' @return An S3 object containing the proportion of specific agreement.
#' @export
#'
#' @examples
#' #' df <- data.frame(r1=factor(c(1,2,2,0,3,3,1,0,3,0,2,2,0,3,1)),
#'                  r2=factor(c(1,1,1,0,3,3,1,0,1,0,2,2,0,2,1)),
#'                  r3=factor(c(1,1,1,3,3,2,1,0,1,0,2,2,0,3,1)),
#'                  r4=factor(c(1,2,1,0,3,3,1,0,3,0,2,2,0,2,1)))
#' x <- sumtable(df=df, ratings=c("r1", "r2", "r3", "r4"), levels=c("0","1", "2", "3"))
#' specific.agreement(x)
specific.agreement <- function(data, cat1, cat2=NULL, ratings=NULL, levels=NULL, offdiag=NULL){
  if(is.data.frame(data)){
    table <- Agree::sumtable(data,ratings=ratings,levels=levels, offdiag = TRUE)
  }
  if(nrow(data)==ncol(data)){
    table <- data
    if(is.null(colnames(table))){levels <- 1:nrow(table)}
  }
  mat1 <- table*0
  if(is.null(colnames(table))){levels <- 1:nrow(table)}
  if(is.null(cat2)){ #if no cat2, then cat1 versus all others
  SA <- (2*table[cat1,cat1]) / (2*table[cat1,cat1] + (sum(table[,cat1])-table[cat1,cat1])+(sum(table[cat1,])-table[cat1,cat1]))
  }
  if(!is.null(cat2)){#if there is a cat2, then cat1 versus cat2
  SA <- (2*table[cat1,cat1]) / (2*table[cat1,cat1] + table[cat1,cat2]+ table[cat2,cat1])
  }
  SA
}
