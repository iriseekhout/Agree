#' ICC oneway
#'
#' @param model merMod object result from \code{icc_model()}.
#' @param alpha confidence interval level, default \code{alpha = 0.05}.
#' @details The multilevel model specification for the oneway agreement is
#'   $Y_{ij} = \beta_{0s} + \epsilon_{ij}$, where $\beta_{0j}$ is the random
#'   intercept at the subject (i.e. id) level and $\epsilon_{ij}$ the residual
#'   error. (\code{lme4}): \code{lmer(score ~ (1|id))}. Accordingly the model
#'   only adjusts for the repeated observations within a subject with the rand
#'   intercept at the subject level. In the \code{icc_oneway()}, the
#'   \code{icc_model()} function is used, which applies the following general
#'   multilevel model to the data $Y_{ijk} = \beta_{0jk} + \beta_{0k} +
#'   \epsilon_{ijk}$, where $\beta_{0jk}$ is the random intercept at the subject
#'   level, $\beta_{0k}$ is the random intercept at the observer (i.e. repeated
#'   rating) level and $\epsilon_{ijk}$ is the residual error. \code{lme4}:
#'   \code{lmer(score ~ (1|observer) + (1|id))}. From the general model, the
#'   variance components for the one-way icc are extracted as follows, to
#'   calculate the icc. $\sigma^{2}_{0j} = \frac{({v}*{\sigma^{2}_{0jk}}) -
#'   \sigma^{2}_{0k}}{v}$, where $v$ is the number of observers (or raters) in
#'   the general model. $\varepsilon_{ij} = \sigma^{2}_{0k} +
#'   \varepsilon_{ijk}$. Then $icc_{oneway} =
#'   \frac{\sigma^{2}_{0j}}{\sigma^{2}_{0j} + \varepsilon_{ij}}$. And
#'   $sem_{oneway} = \sqrt{\varepsilon_{ij}}$. The F-statistic for computing the
#'   confidence intervals is obtained as follows: $F_{oneway} = \frac{v
#'   *\sigma^{2}_{0j} + \varepsilon_{ij}}{\varepsilon_{ij}}$. And the following
#'   intervals are computed as: $l,u = \frac{F_{oneway}/F_l -1}{F_{oneway}/F_l +
#'   v - 1}, \frac{F_{oneway}/F_u -1}{F_{oneway}/F_u + v - 1}$, with $F_l$ and
#'   $F_u$ as the $\alpha$/2 and 1-$\alpha$/2 quantiles of the F-distribution
#'   with $df1 = n - 1$ and $df2 = n(v -1)$.
#' @importFrom lme4 ngrps VarCorr
#' @return
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
