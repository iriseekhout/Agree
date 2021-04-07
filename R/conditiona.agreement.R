#' Conditional agreement for more than 2 categories
#'
#' Conditional agreement when there are more than two categories (averages over discordant cells to correct for random rater combinations). INCLUDE FORMULAS.
#'
#' @param data A data matrix or table with equal number of columns and rows.
#' @param \dots options for sumtable
#' @return conditionaltable is a table with conditional agreement proportions. On the diagonal the specific agreement proportions for each category are displayed.
#' @export
#'
#' @examples
#' df <- data.frame(r1=factor(c(1,2,2,0,3,3,1,0,3,0,2,2,0,3,1)),
#'                  r2=factor(c(1,1,1,0,3,3,1,0,1,0,2,2,0,2,1)),
#'                  r3=factor(c(1,1,1,3,3,2,1,0,1,0,2,2,0,3,1)),
#'                  r4=factor(c(1,2,1,0,3,3,1,0,3,0,2,2,0,2,1)))
#' conditional.agreement(df)
conditional.agreement <- function(data, ...){
  if(is.data.frame(data)){
    table <- Agree::sumtable(data, ...)
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
