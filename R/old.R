#' positive agreement-deprecated
#'
#' A function to obtain the positive or negative agreement between 2 or more raters when categories are dichotomous.
#'
#' @param data A data matrix or table with equal number of columns and rows. Or a data frame that contains the scores for each rater in each column.
#' @param specific A character vector indicating whether the \code{"positive"} or \code{"negative"} agreements should be obtained.
#' @param \dots optoins for sumtable.
#'
#' @return An S3 object containing the proportion of positive (or negative) agreement.
#'
#' @name positive.agreement-deprecated
#' @usage positive.agreement(data, specific, ...)
#' @seealso \code{\link{Agree-deprecated}}
#' @keywords internal
NULL
#' @rdname Agree-deprecated
#' @section \code{positive.agreement}:
#' For \code{positive.agreement}, use \code{\link{agreement}}.
#'
#' @export
positive.agreement <- function(data, specific="positive", ...){
  .Deprecated("agreement", old = "positive.agreement")
  agreement(data = data, specific = specific, ...)
}


#' Specific agreement-deprecated
#'
#' specific agreement (averages over discordant cells to correct for random rater combinations). INCLUDE FORMULAS. When there are 2 categories, this is equal to the postive/negative agreement. When there are more than two categories, one can either look at the agreement for one category versus the others, for example very satisfied verus rest or for one category versus one specific other categorie, for example very satisfies versus not satisfied.
#'
#' @param data A data matrix or table with equal number of columns and rows. Or a data frame that contains the scores for each rater in each column.
#' @param cat1 A character indicating the category for which specific agreement should be obtained.
#' @param cat2 A character indicating the category to which the specific agreement should be compared, if left empty all other categories are used.
#' @param \dots options for sumtable
#'
#' @return An S3 object containing the proportion of specific agreement.
#' @name specific.agreement-deprecated
#' @usage specific.agreement(data, cat1, cat2, ...)
#' @seealso \code{\link{Agree-deprecated}}
#' @keywords internal
NULL
#' @rdname Agree-deprecated
#' @section \code{specific.agreement}:
#' For \code{specific.agreement}, use \code{\link{agreement}}.
#'
#' @export
specific.agreement <- function(data, cat1, cat2=NULL, ...){
  .Deprecated("agreement")
  specific <- c(cat1, cat2)
  agreement(data = data, specific = specific, ...)
}

#' Confidence interval around the (specific) agreement
#'
#' The confidence interval estimated around the proportion of agreement between 2 or more raters. If cat1 is defined then specific agreement will be calculated. If cat1 = NULL overall agreement will be calculated. For the overall agreement and specific agreement if the number of likert categories is 2, the confidence interval will be obtained with the formula (Fleis correction for the values close to 0 or 1 and continuity correction for the other end of the interval). When the number of categories is larger than 2, the bootstrapped confidence interval will be obtained.
#'
#' @param data data.frame or table
#' @param cat1 A character indicating the category for which specific agreement should be obtained.
#' @param cat2 A character indicating the category to which the specific agreement should be compared, if left empty all other categories are used.
#' @param interval Confidence level; default is 0.95.
#' @param m number of categories
#' @param n sample size
#' @param b number of boostrap iterations
#'
#' @return A vector giving the lower and upper confidence limit around the probability of specific agreement.
#' @name CIagreement-deprecated
#' @usage CIagreement(data, interval=0.95, cat1=NULL, cat2=NULL,m=NULL,n=NULL,b=1000)
#' @seealso \code{\link{Agree-deprecated}}
#' @keywords internal
NULL
#' @rdname Agree-deprecated
#' @section \code{CIagreement}:
#' For \code{CIagreement}, use \code{\link{agreement}}.
#'
#' @export
 CIagreement <- function(data, interval=0.95, cat1=NULL, cat2=NULL,m=NULL,n=NULL,b=1000){

 .Deprecated("agreement")

  stopifnot(interval>=0|interval<=1)
  if(is.data.frame(data)){
    table <- Agree::sumtable(data, offdiag = TRUE)
    m=ncol(data)
    n=nrow(data)
  }
  if(nrow(data)==ncol(data)){
    table <- data
    ncat <- ncol(table)
    if(is.null(colnames(table))){levels <- 1:nrow(table)}
    if(ncat>2 & !is.null(cat1)){#if table input and more than 2 categories and specific agreement
      stop("input should be a data.frame to obtain confidence intervals for specific agreement with more than 2 factor levels")}
    if(is.null(n)){stop("n not found")}
    if(is.null(m)){stop("m not found")}
  }
  ncat <- ncol(table)

  #ordinary agreement
  if(is.null(cat1)&is.null(cat2)){
    p <- Agree::agreement(table)
    n <- n*sqrt(m-1)
  }
  #specific agreement versus all others (n for ncat=2)
  if (!is.null(cat1) & is.null(cat2)){ #if no cat2, then cat1 versus all others
    p <- Agree::agreement(table,specific= c(cat1))
    n <- ((table[cat1,cat1]+(sum(table[cat1,])-table[cat1,cat1]))/(m*(m-1)/2))*sqrt(m-1)
  }
  #specific agreement versus cat2 (n for ncat=2)
  if(!is.null(cat1) & !is.null(cat2)){#if there is a cat2, then cat1 versus cat2
    p <- Agree::agreement(table,specific=c(cat1,cat2))
    n <- ((((table[cat1,cat1]+table[cat1,cat2])+(table[cat1,cat1]+table[cat2,cat1]))/2) /(m*(m-1)/2))*sqrt(m-1)
  }
  if(ncat==2|is.null(cat1)){
    a <- qnorm(1-((1-interval)/2))
    CCI<-  p+c(-(a*(sqrt(1/n*(p*(1-p))))-1/(2*n)),0, (a*(sqrt(1/n*(p*(1-p))))+1/(2*n))) #continuity
    FCIlow <- ((2*n*p+(a*a)-1)-a*sqrt((a*a)-(2+(1/n))+4*p*(n*(1-p)+1)))/(2*((a*a)+n)) #Fleis
    FCIhigh <- ((2*n*p+(a*a)-1)+a*sqrt((a*a)-(2+(1/n))+4*p*(n*(1-p)+1)))/(2*((a*a)+n))#Fleis
    if (p<0.5){CI <-  c(CIlow=FCIlow, agreement=p, CIhigh=CCI[3])}
    if (p>=0.5){CI <-  c(CIlow=CCI[1], agreement=p, CIhigh=FCIhigh)}
  }
  if(ncat>2 & !is.null(cat1)){
    agree.boot <- function(data,x) {
      Agree::agreement(sumtable(data[x,]),specific = c(cat1,cat2))}
    res1a <- boot::boot(data,agree.boot,b)
    BCI_agr <-  quantile(res1a$t,c((1-interval)/2,interval+((1-interval)/2)), na.rm=TRUE)
    CI <- c(BCIlow=BCI_agr[1], agreement=p, BCIhigh=BCI_agr[2])
  }
  CI
}



#' Bootstrapped confidence interval around the Agreement
#'
#' The confidence interval estimated around the proportion of agreement between 2 or more raters.
#'
#' @param data data.frame or table
#' @param cat1 A character indicating the category for which specific agreement should be obtained.
#' @param cat2 A character indicating the category to which the specific agreement should be compared, if left empty all other categories are used.
#' @param alpha Confidence level; default is 0.95.
#' @param b Number of bootstrap iterations.
#'
#' @return A vector giving the lower and upper confidence limit around the probability.
#' @export
#'
#' @examples
#' df <- data.frame(r1=factor(c(1,0,1,0,0,1,1,0,0,0,1,1,0,1,1)),
#'                  r2=factor(c(1,1,1,1,0,1,1,0,0,0,1,1,0,1,0)),
#'                  r3=factor(c(1,1,1,0,0,0,1,1,1,0,0,1,0,1,1)),
#'                  r4=factor(c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1)))
#' CIbootagreement(data=df)
#'
#'
CIbootagreement <- function(data, cat1=NULL, cat2=NULL,b=1000, alpha=0.05){
  stopifnot(alpha>=0|alpha<=1)
  if(!is.null(cat1)){
    p <- agreement(sumtable(data), specific = c(cat1,cat2))
    agree.boot <- function(data,x) {
      Agree::agreement(sumtable(data[x,]), specific = c(cat1, cat2))
    }
  }

  if(is.null(cat1)){
    p <- agreement(sumtable(data))
    agree.boot <- function(data,x) {
      Agree::agreement(sumtable(data[x,]))}
  }
  res1a <- boot::boot(data,agree.boot,b)
  BCI_agr <-  quantile(res1a$t,c((alpha)/2,(1-alpha)+((alpha)/2)), na.rm=TRUE)    # Bootstrapped confidence interval of Light's kappa
  BCI_agree <- BCI_agr   #,BCIadj_agr$bca[4:5])
  BCI <- c(BCIlow=BCI_agree[1], agreement=p, BCIhigh=BCI_agree[2])
  BCI
}


