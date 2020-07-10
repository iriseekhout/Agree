#' ICC oneway
#'
#' A function that computes the one-way ICC, with the corresponding standard error of measurement, sem. And the confidence intervals. see details for more information.
#'
#' @param model merMod object result from \code{icc_model()}.
#' @param alpha confidence interval level, default \code{alpha = 0.05}.
#' @return a list with parameter estimates.
#' @importFrom lme4 ngrps VarCorr
#' @export
icc_oneway <- function(model, alpha = 0.05){
  k <- lme4::ngrps(model)[2]
  n <- lme4::ngrps(model)[1]
  vc <- as.data.frame(lme4::VarCorr(model))

  #oneway
  varpat_oneway <- ((k * vc[1,4]) - vc[2,4]) / k
  varerr_oneway <- (vc[2,4] + vc[3,4])
  icc_o <- varpat_oneway / (varpat_oneway + varerr_oneway)
  sem_o <- sqrt(varerr_oneway)
  F_o <- (k * varpat_oneway + varerr_oneway)/varerr_oneway
  dfon <- n - 1
  dfod <- n * (k - 1)
  F_oL <- F_o/qf(1 - alpha/2, dfon, dfod) #or alpha/2?not dividing by 2 is shrout fleis
  F_oU <- F_o * qf(1 - alpha/2, dfod, dfon) #or alpha/2?
  L_o <- (F_oL - 1)/(F_oL + (k - 1))
  U_o <- (F_oU - 1)/(F_oU + k - 1)

  return(
    list(
      varpat_oneway = varpat_oneway,
      varerr_oneway = varerr_oneway,
      icc_o = icc_o,
      sem_o = sem_o,
      L_o = L_o,
      U_o = U_o
    )
  )
}
