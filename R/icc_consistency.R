#' ICC consistency
#'
#' The intraclass correlations (ICC) of consistency for rater reliability using
#' the variance estimates from a linear mixed model. The function returns the
#' ICC, standard error of measurment (sem) and confidence intervals for ICC.
#'
#' @param model merMod object result from `icc_model()`.
#' @param alpha confidence interval level, default `alpha = 0.05`.
#' @importFrom lme4 ngrps VarCorr
#' @return
#' @export
#' @details
#' The ICC type consistency is the variance between the subjects divided by the sum
#' of the subject variance and the residual variance. The subject variance and
#' error variance are adjusted for the rater variance, but the rater variance is
#' not used to calculate the ICC. The ICC for consistency generalizes only to
#' the fixed set of raters in the data (Shrout & Fleiss,
#' 1979). The `icc_model()` function is used to compute the variances.
#' This is a `lmer` model with a random slope for the subjects as well as for
#' the raters. The sem is the square root of the sum of the rater variance and
#' the error variance.
#' The confidence are computed with the exact F method. F = (k * subject variance +
#' error variance)/ error variance, with df1 = n - 1 and df2 = (n - 1) * (k - 1)
#' (Shrout & Fleiss, 1979).
#' @author Iris Eekhout
#' @references
#' Fleiss, J. L., & Shrout, P. E. Approximate interval estimation for a certain
#' intraclass correlation coefficient. Psychometrika, 1978, 43, 259-262.
icc_consistency <- function(model, alpha = 0.05){
  k <- lme4::ngrps(model)[2]
  n <- lme4::ngrps(model)[1]
  vc <- as.data.frame(lme4::VarCorr(model))

  #consistency
  varpat_cons <- vc[1,4]
  varerr_cons <- vc[3,4]
  icc_c <- varpat_cons/(varpat_cons + varerr_cons)
  sem_c <- sqrt(varerr_cons)
  F_c <- (k * varpat_cons + varerr_cons)/varerr_cons
  df21n <- n - 1
  df21d <- (n - 1) * (k - 1)
  F3L <- F_c/qf(1 - alpha/2, df21n, df21d) #or alpha/2? not dividing by 2 is shrout fleis
  F3U <- F_c * qf(1 - alpha/2, df21d, df21n)#or alpha/2?
  L_c <- (F3L - 1)/(F3L + k - 1)
  U_c <- (F3U - 1)/(F3U + k - 1)

  return(
    list(
      varpat_cons = varpat_cons,
      varerr_cons = varerr_cons,
      icc_c = icc_c,
      sem_c = sem_c,
      L_c = L_c,
      U_c = U_c
    )
  )
}
