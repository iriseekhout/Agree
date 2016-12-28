
#' Agreement
#'
#' The proportion of overall agreement between 2 or more raters is calculated by INCLUDE FORMULA.
#'
#' @param table A data matrix or table with equal number of columns and rows.
#'
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
agreement <- function(table){
  stopifnot(nrow(table)==ncol(table))
  agreement <- sum(diag(table)/sum(table))
  agreement
}

#' Specific agreement
#'
#' A function to obtain the positve or negative agreement between 2 or more raters when categories are dichotomous. INCLUDE FORMULAS.
#'
#' @param table A 2 x 2 data matrix or table.
#' @param specific A character vector indicating whether the \code{"positive"} or \code{"negative"} agreements should be obtained.
#'
#' @return An S3 object containing the proportion of specific agreement.
#' @export
#'
#' @examples
#' #' df <- data.frame(r1=factor(c(1,0,1,0,0,1,1,0,0,0,1,1,0,1,1)),
#'                  r2=factor(c(1,1,1,1,0,1,1,0,0,0,1,1,0,1,0)),
#'                  r3=factor(c(1,1,1,0,0,0,1,1,1,0,0,1,0,1,1)),
#'                  r4=factor(c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1)))
#' table <- sumtable(df=df, ratings=c("r1", "r2", "r3", "r4"), levels=c("0","1"))
#' specific.agreement1(table, specific="positive")
specific.agreement1 <- function(table, specific="positive"){
  stopifnot(nrow(table)==2 & ncol(table)==2)
if(specific=="positive"){
  specific.agreement <- (2*(table[1,1]))/((2*(table[1,1]))+(table[1,2])+(table[2,1]))
}
if(specific=="negative"){
  specific.agreement <- (2*(table[2,2]))/((2*(table[2,2]))+(table[1,2])+(table[2,1]))
}
specific.agreement
}

#' Agreement plus/minus one category
#'
#' The agreement between 2 or more raters when they may be one category off. Only relevant for ordinal rating scales with more than 2 Likert categories. INCLUDE FORMULAS.
#'
#' @param table A data matrix or table with equal number of columns and rows.
#'
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
agreement.plusone <- function(table){
  stopifnot(nrow(table)==ncol(table))
  stopifnot(nrow(table>2))
  agreeup <- agreelow <- vector()
  for (i in 1:(ncol(table) - 1)) {
    agreeup[i] <- table[i,i + 1]
    agreelow[i] <- table[i + 1,i]
  }
  agreement.plusone <-  (sum(diag(table),agreeup, agreelow) / sum(table))
  agreement.plusone
}


#' Specific agreement for more than 2 categories
#'
#' specific agreement when there are more than two categories (averages over discordant cells to correct for random rater combinations). INCLUDE FORMULAS.
#'
#' @param table A data matrix or table with equal number of columns and rows.
#'
#' @return betweentable The same as the input table, where the discordant cells are averaged to correct for random rater combinations.
#' @return specific.agreements
#' @return specific.agreements.prop
#' @export
#'
#' @examples
#' #' df <- data.frame(r1=factor(c(1,2,2,0,3,3,1,0,3,0,2,2,0,3,1)),
#'                  r2=factor(c(1,1,1,0,3,3,1,0,1,0,2,2,0,2,1)),
#'                  r3=factor(c(1,1,1,3,3,2,1,0,1,0,2,2,0,3,1)),
#'                  r4=factor(c(1,2,1,0,3,3,1,0,3,0,2,2,0,2,1)))
#' table <- sumtable(df=df, ratings=c("r1", "r2", "r3", "r4"), levels=c("0","1", "2", "3"))
#' specific.agreement2(table)
specific.agreement2 <- function(table){
  stopifnot(nrow(table)==ncol(table))
  mat1 <- matrix(0,nrow(table),ncol(table))
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
  betweentable(round(betweentable),3)
  specific.agreements <- mat1.2_colprop[1:length(levels)]

  mat2 <- matrix (0,length(levels) ,length(levels) )
  for (i in seq_along(levels)) {
    mat2[i,] <- mat1[i,] / mat1_rowtot[i]
  }
  prop_agree_cat <- round(mat2, 3)
  rownames(prop_agree_cat) <- c(paste(levels))
  colnames(prop_agree_cat) <- c(paste(levels))

  list(betweentable=betweentable, specific.agreements=specific.agreements, specific.agreements.prop=prop_agree_cat)
}
