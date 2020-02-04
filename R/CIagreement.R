#' Confidence interval around the (specific) agreement
#'
#' The confidence interval estimated around the proportion of agreement between 2 or more raters. If cat1 is defined then specific agreement will be calculated. If cat1 = NULL overall agreement will be calculated. For the overall agreement and specific agreement if the number of likert categories is 2, the confidence interval will be obtained with the formula (Fleis correction for the values close to 0 or 1 and continuity correction for the other end of the interval). When the number of categories is larger than 2, the bootstrapped confidence interval will be obtained.
#'
#' @param data data.frame or table
#' @param cat1 A character indicating the category for which specific agreement should be obtained.
#' @param cat2 A character indicating the category to which the specific agreement should be compared, if left empty all other categories are used.
#' @param interval Confidence level; default is 0.95.
#'
#' @return A vector giving the lower and upper confidence limit around the probability of specific agreement.
#' @export
#'
#' @examples
#' df <- data.frame(r1=factor(c(1,0,1,0,0,1,1,0,0,0,1,1,0,1,1)),
#'                  r2=factor(c(1,1,1,1,0,1,1,0,0,0,1,1,0,1,0)),
#'                  r3=factor(c(1,1,1,0,0,0,1,1,1,0,0,1,0,1,1)),
#'                  r4=factor(c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1)))
#' CIagreement(data=df, n=nrow(df), m=ncol(df))
#'
CIagreement <- function(data, interval=0.95, ratings=NULL, levels=NULL, cat1=NULL, cat2=NULL,m=NULL,n=NULL,b=1000){
  stopifnot(interval>=0|interval<=1)
  if(is.data.frame(data)){
    table <- Agree::sumtable(data,ratings=ratings,levels=levels, offdiag = TRUE)
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
    p <- Agree::specific.agreement(table,cat1=cat1,cat2=NULL)
    n <- ((table[cat1,cat1]+(sum(table[cat1,])-table[cat1,cat1]))/(m*(m-1)/2))*sqrt(m-1)
  }
  #specific agreement versus cat2 (n for ncat=2)
  if(!is.null(cat1) & !is.null(cat2)){#if there is a cat2, then cat1 versus cat2
    p <- Agree::specific.agreement(table,cat1=cat1,cat2=cat2)
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
      Agree::specific.agreement(sumtable(data[x,]),cat1=cat1,cat2=cat2)}
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
#' @param level Confidence level; default is 0.95.
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
CIbootagreement <- function(data, cat1=NULL, cat2=NULL,b=1000, interval=0.95){
  stopifnot(interval>=0|interval<=1)
  if(!is.null(cat1)){
    p <- specific.agreement(sumtable(data),cat1=cat1,cat2=cat2)
    agree.boot <- function(data,x) {
      Agree::specific.agreement(sumtable(data[x,]),cat1=cat1,cat2=cat2)
    }
  }

  if(is.null(cat1)){
    p <- agreement(sumtable(data))
    agree.boot <- function(data,x) {
      Agree::agreement(sumtable(data[x,]))}
    }
    res1a <- boot::boot(data,agree.boot,b)
    BCI_agr <-  quantile(res1a$t,c((1-interval)/2,interval+((1-interval)/2)), na.rm=TRUE)    # Bootstrapped confidence interval of Light's kappa
  BCI_agree <- BCI_agr   #,BCIadj_agr$bca[4:5])
  BCI <- c(BCIlow=BCI_agree[1], agreement=p, BCIhigh=BCI_agree[2])
  BCI
}

