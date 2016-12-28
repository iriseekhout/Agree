
#' Confidence interval around the Agreement
#'
#' The confidence interval estimated around the proportion of agreement between 2 or more raters.
#'
#' @param p Proportion; a numeric value between 0 and 1
#' @param n The sample size of the sample that is rated
#' @param m The number of raters
#' @param level Confidence level; default is 0.95.
#' @param correction A character value indicating the method used for correction of the interval, options are "continuity" or "Fleis".
#'
#' @return A vector giving the lower and upper confidence limit around the probability.
#' @export
#'
#' @examples
#' df <- data.frame(r1=factor(c(1,0,1,0,0,1,1,0,0,0,1,1,0,1,1)),
#'                  r2=factor(c(1,1,1,1,0,1,1,0,0,0,1,1,0,1,0)),
#'                  r3=factor(c(1,1,1,0,0,0,1,1,1,0,0,1,0,1,1)),
#'                  r4=factor(c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1)))
#' table <- sumtable(df=df, ratings=c("r1", "r2", "r3", "r4"), levels=c("0","1"))
#' p <- agreement(table)
#' CIagreement(p=p, n=nrow(df), m=ncol(df))
#'
CIagreement <- function(p, n, m, level=0.95, correction="continuity"){
   stopifnot(p>=0 | p<=1)
  stopifnot(is.numeric(p))
  stopifnot(is.numeric(n))
  stopifnot(is.numeric(m))
  stopifnot(level>=0|level<=1)
  stopifnot(is.character(correction))
  a <- qnorm(1-((1-level)/2))
  n <- n*sqrt(m-1)
  if (correction=="continuity"){
  CIagreement<-  p+c(-(a*(sqrt(1/n*(p*(1-p))))-1/(2*n)),0, (a*(sqrt(1/n*(p*(1-p))))+1/(2*n)))
  CIagreement <-  c(CIlow=CIagreement[1], p=CIagreement[2], CIhigh=CIagreement[3])

  }
  if (correction=="Fleis"){
    FCIlow <- ((2*n*p+(a*a)-1)-a*sqrt((a*a)-(2+(1/n))+4*p*(n*(1-p)+1)))/(2*((a*a)+n))
    FCIhigh <- ((2*n*p+(a*a)-1)+a*sqrt((a*a)-(2+(1/n))+4*p*(n*(1-p)+1)))/(2*((a*a)+n))
  CIagreement <-  c(CIlow=FCIlow, p=p, CIhigh=FCIhigh)
  }
  CIagreement
}
