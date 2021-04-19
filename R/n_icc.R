#' Sample size for ICC
#'
#' @param beta power level, default \code{beta = 0.8}
#' @param alpha confidence level, default \code{alpha = 0.05}
#' @param k number of raters, default \code{k = 3}
#' @param icc reliability coefficient
#' @param icc_lower lower limit of the reliability coefficient icc
#' @details Calculate the sample size for achieving a certain lower limit of the
#' confidence interval derived by the F procedure. This method is developed for
#' the ICC type consistency.
#' @references
#' Zou, G.Y. (2012) Sample size formulas for estimating intraclass correlation
#' coefficients with precision and assurance. Statistics in medicine, 31, 3971-3981.
#' @export
#' @examples
#' n_icc(icc = 0.7, icc_lower = 0.6, k = 4)
#' n_icc(icc = 0.7, icc_lower = 0.6, k = 2)
#' n_icc(icc = 0.7, icc_lower = 0.6, k = 3)
n_icc <- function(beta = 0.8,
                  alpha = 0.05,
                  k = 3,
                  icc,
                  icc_lower
){
  za <- qnorm(alpha, lower.tail = FALSE)
  zb <- qnorm(beta, lower.tail = TRUE)
  Fp <- (1+(k-1)*icc)/(1-icc)
  Fpo <- (1+(k-1)*icc_lower)/(1-icc_lower)
  n <- 1 + ((2*(zb + za)^2 * k)/ ((log(Fp/Fpo))^2 *(k-1)))

  #solve for k: k <- ((n-1) * ((log(Fp/Fpo))^2)) / ((n-1) * (log(Fp/Fpo))^2) - 2 * (za + zb)^2
  n
}
