#' ICC consistency
#'
#' @param model merMod object result from \code{icc_model()}.
#' @param alpha confidence interval level, default \code{alpha = 0.05}.
#' @details lmer(score ~ observer + (1|id), data1, REML = T)
#' @importFrom lme4 ngrps VarCorr
#' @return
#' @export
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
  F3L <- F_c/qf(1 - alpha, df21n, df21d) #or alpha/2? not dividing by 2 is shrout fleis
  F3U <- F_c * qf(1 - alpha, df21d, df21n)#or alpha/2?
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
