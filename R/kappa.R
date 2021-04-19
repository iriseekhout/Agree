#' Kappa for agreement with multiple raters
#'
#' @param data A `data.frame` or table with equal number of columns and rows. Or
#' `data.frame` that contains the scores for each rater in each column.
#' @param confint Logical indicator for confidence interval
#' @param alpha Confidence interval level, default = 0.05.
#' @param k number of raters; default `k = ncol(data)`.
#' @param n sample size; default `n = nrow(data)`.
#' @param \dots options for sumtable if `is.data.frame(data)`
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
           k = NULL,
           n = NULL,
           ...){
    if(is.data.frame(data)){
      table <- Agree::sumtable(data,...)
      k <- ncol(data)
      n <- nrow(data)
    }
    if(nrow(data)==ncol(data)){
      table <- data
     # if(confint & (is.null(k) | is.null(n))){
      #  warning("Confidence intervals cannot be computed, k and n are missing.")
      #  confint <- FALSE
      #}
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

    #based on Variance:
    #n_ad <- n*sqrt(k-1) #adjusted n for multiple raters
    im <- diag(1, ncol(table), nrow(table)) #identity matrix
    Variance <-(1 / (tot * (1 - pe) ^ 4)) * (sum(diag(x * (im * (1 - pe) -
                (outer(rs, cs, FUN = "+")) * (1 - po))^2)) + (1 - po)^2 * (sum(x *
                (outer(cs, rs, FUN = "+"))^2) - sum(diag(x * (outer(cs, rs, FUN = "+"))^2))) - (po *
                pe - 2 * pe + po)^2)


    if(confint){
      #via variance computation
     # if(k == 2){
      CI <- c(lower = kappa - qnorm(1-(alpha/2)) * sqrt(Variance),
              upper = kappa + qnorm(1-(alpha/2)) * sqrt(Variance))
       kappa <- c(kappa, CI) #ci disabled for publication - first check if correct.
      #}
      #if(k > 2 ){
      #  warning("Confidence intervals for more than 2 rater cannot be estimated directly for kappa. Use bootCI for bootstrapped intervals.")
      #}

    }





    return(kappa)

  }

# Notes: CI tested - if computed as CI for p-value, the intervals are off. I am now using Ci via variance computation. The totals (n) is computed from the table sum. When compared to bootstrap with bootCI the intervals are sometimes shifted to the right - can be problematic. Needs more testing to be sure. If you want to be conservative - use bootCI.
