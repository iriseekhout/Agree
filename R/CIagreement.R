#' Confidence interval around the (specific) agreement
#'
#' The confidence interval estimated around the proportion of agreement between 2 or more raters. If cat1 is defined then specific agreement will be calculated. If cat1 = NULL overall agreement will be calculated.
#'
#' @param data
#' @param cat1 A character indicating the category for which specific agreement should be obtained.
#' @param cat2 A character indicating the category to which the specific agreement should be compared, if left empty all other categories are used.

#' @param level Confidence level; default is 0.95.
#' @param correction A character value indicating the method used for correction of the interval, options are "continuity" or "Fleis".
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
CIagreement <- function(data, level=0.95, correction="continuity", ratings=NULL, levels=NULL, cat1=NULL, cat2=NULL,m=NULL,n=NULL ){
  stopifnot(level>=0|level<=1)
  stopifnot(is.character(correction))
  if(is.data.frame(data)){
    table <- Agree::sumtable(data,ratings=ratings,level=levels, offdiag = TRUE)
    m=ncol(data)
    n=nrow(data)
  }
  if(nrow(data)==ncol(data)){
    table <- data
  }
  #calculate specific agreement
  mat1 <- table*0
  if(is.null(colnames(table))){
    levels <- 1:nrow(table)
    }
  #ordinary agreement
  if(is.null(cat1)&is.null(cat2)){
    p <- Agree::agreement(table)
    n <- n*sqrt(m-1)
    }
  #specific agreement versus all others
  if (!is.null(cat1) & is.null(cat2)){ #if no cat2, then cat1 versus all others
    #p <- (2*table[cat1,cat1]) / (2*table[cat1,cat1] + (sum(table[,cat1])-table[cat1,cat1])+(sum(table[cat1,])-table[cat1,cat1]))
    p <- Agree::specific.agreement(table,cat1=cat1,cat2=NULL)
    n <- ((table[cat1,cat1]+(sum(table[cat1,])-table[cat1,cat1]))/(m*(m-1)/2))*sqrt(m-1)
    #verschil met eerdere simulatie komt doordat hier de offdiag means worden genomen en daar niet - dit nu uitgezet.
  }
  #specific agreement versus cat2
  if(!is.null(cat1) & !is.null(cat2)){#if there is a cat2, then cat1 versus cat2
    #p <- (2*table[cat1,cat1]) / (2*table[cat1,cat1] + table[cat1,cat2]+ table[cat2,cat1])
    p <- Agree::specific.agreement(table,cat1=cat1,cat2=cat2)
    n <- ((((table[cat1,cat1]+table[cat1,cat2])+(table[cat1,cat1]+table[cat2,cat1]))/2) /(m*(m-1)/2))*sqrt(m-1)
  }

  #obtain CI
  a <- qnorm(1-((1-level)/2))
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



#' Bootstrapped confidence interval around the Agreement
#'
#' The confidence interval estimated around the proportion of agreement between 2 or more raters.
#'
#' @param data
#' @param cat1
#' @param cat2
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
CIbootagreement <- function(data, cat1=NULL, cat2=NULL,b=1000, level=0.95){
  stopifnot(level>=0|level<=1)
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
    BCI_agr <-  quantile(res1a$t,c((1-level)/2,level+((1-level)/2)), na.rm=TRUE)    # Bootstrapped confidence interval of Light's kappa
  BCI_agree <- BCI_agr   #,BCIadj_agr$bca[4:5])
  BCI <- c(BCIlow=BCI_agree[1], est=p, BCIhigh=BCI_agree[2])
  BCI
}

