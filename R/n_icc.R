#' Sample size for ICC
#'
#' @param beta power level, default \code{beta = 0.8}
#' @param alpha confidence level, default \code{alpha = 0.05}
#' @param k number of raters, default \code{k = 3}
#' @param icc reliability coefficient
#' @param icc_lower lower limit of the reliability coefficient icc
#' @return
n_icc <- function(beta = 0.8,
                  alpha = 0.05,
                  k = 3,
                  icc,
                  icc_lower
){
  za <- qnorm(alpha, lower = FALSE)
  zb <- qnorm(beta, lower = TRUE)
  Fp <- (1+(k-1)*icc)/(1-icc)
  Fpo <- (1+(k-1)*icc_lower)/(1-icc_lower)
  n <- 1 + ((2*(zb + za)^2 *3)/ ((log(Fp/Fpo))^2 *(k-1)))
  n
}

## also add the calculations for the achievable lower limit Po -- see ZHU

#In addition, it is straightforward to derive the achievable lower limit for given values of N , ρ , k , α , and β . Let


#The achievable lower limit ρ 0 is given by
