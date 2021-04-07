#' Kappa for agreement with multiple raters
#'
#' @param data A data frame or table with equal number of columns and rows. Or a data frame that contains the scores for each rater in each column.
#' @param confint Logical indicator for confidence interval
#' @param alpha Confidence level, default = 0.05.
#' @param k number of raters; default \code{k = ncol(data)}
#' @param n sample size; default \code{n = nrow(data)}
#' @param \dots options for sumtable if \code{data = data.frame}
#'
#' @return
#' @export
#' @examples
#' df <- data.frame(r1=factor(c(1,0,1,0,0,1,1,0,0,0,1,1,0,1,1)),
#'                  r2=factor(c(1,1,1,1,0,1,1,0,0,0,1,1,0,1,0)),
#'                  r3=factor(c(1,1,1,0,0,0,1,1,1,0,0,1,0,1,1)),
#'                  r4=factor(c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1)))
#' table <- sumtable(df=df, ratings=c("r1", "r2", "r3", "r4"), levels=c("0","1"))
#' kappa(df)
#' kappa(table)
#'
#' df <- data.frame(r1=factor(c(1,2,2,0,3,3,1,0,3,0,2,2,0,3,1)),
#'                  r2=factor(c(1,1,1,0,3,3,1,0,1,0,2,2,0,2,1)),
#'                  r3=factor(c(1,1,1,3,3,2,1,0,1,0,2,2,0,3,1)),
#'                  r4=factor(c(1,2,1,0,3,3,1,0,3,0,2,2,0,2,1)))
#' table <- sumtable(df=df, ratings=c("r1", "r2", "r3", "r4"), levels=c("0","1", "2", "3"))
#' kappa(df)
#' kappa(table)
#' kappa(df, confint = TRUE)
#' kappa(table)
kappa <-
  function(data,
           confint = FALSE,
           alpha = 0.05,
           k = ncol(data),
           n = nrow(data),
           ...){
    if(is.data.frame(data)){
      table <- Agree::sumtable(data,...)
    }
    if(nrow(data)==ncol(data)){
      table <- data
    }

    #formula for kappa = (p0 - pe) / (1-pe)
    #p0 = overall agreement
    #pe = expected agreement
    tot <- sum(table)
    x <- table/tot
    rs <- rowSums(x)
    cs <- colSums(x)
    prob <- rs %*% t(cs)
    po <- sum(diag(x)) #is equal to agreement(table)
    pe <- sum(diag(prob))
    kappa <- (po - pe)/(1 - pe)
    names(kappa) <- "kappa"

    ## General CI function:
    ci <- function(p, n, alpha) {
      a <- qnorm(1 - ((alpha) / 2))
      CCI <-
        p + c(-(a * (sqrt(1 / n * (
          p * (1 - p)
        ))) - 1 / (2 * n)), 0, (a * (sqrt(1 / n * (
          p * (1 - p)
        ))) + 1 / (2 * n))) #continuity
      FCIlow <-
        ((2 * n * p + (a * a) - 1) - a * sqrt((a * a) - (2 + (1 / n)) + 4 * p *
                                                (n * (1 - p) + 1))) / (2 * ((a * a) + n)) #Fleis
      FCIhigh <-
        ((2 * n * p + (a * a) - 1) + a * sqrt((a * a) - (2 + (1 / n)) + 4 * p *
                                                (n * (1 - p) + 1))) / (2 * ((a * a) + n))#Fleis
      if (p < 0.5) {
        CI <-  c(FCIlow, CCI[3])
      }
      if (p >= 0.5) {
        CI <-  c(CCI[1], FCIhigh)
      }
      names(CI) <- c("lower", "upper")
      CI
    }

    if(confint){
      n <- n*sqrt(k-1) #n for CI for overall agreement
      CI <- ci(p = kappa, n = n, alpha = alpha)
      # kappa <- c(kappa, CI) #ci disabled for publication - first check if correct.
    }





    return(kappa)

  }
