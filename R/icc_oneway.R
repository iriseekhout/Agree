#' ICC oneway
#'
#' A function that computes the one-way intraclass correlations (ICC), with the
#' corresponding standard error of measurement, sem, and the confidence
#' intervals, using the variance estimates from a linear mixed model.
#' See details for more information.
#'
#' @param data data.frame with a column for each observer/rater and a row per
#' rated subject.
#' @param cols character vector with the column names to be used as observers.
#' Default is `cols = colnames(data)`.
#' @param alpha confidence interval level, default `alpha = 0.05`.
#' @param twoway logical indicator if the variance components are estimated from
#' the two-way model default: `twoway = FALSE`.
#' @return a `list` with parameter estimates.
#' @importFrom lme4 ngrps VarCorr
#' @export
#' @details
#' The ICC type oneway is the variance between the subjects divided by the sum
#' of the subject variance and the residual variance (total variance in a oneway
#' model. Each subject is rated by a different set of raters, that are randomly
#' selected from a larger population of judges (Shrout & Fleiss, 1979). The
#' `icc_oneway()` uses the `varcomp()` function to compute the variances.
#' Theses variances are estimated from a `lmer` model with random slope for the
#' subjects. When `twoway = TRUE` a level for the raters is estimtaed as well
#' and the rater variance is not used for the ICC oneway and is subtracted from
#' the sum of subject variance over the raters, which is then averaged.
#' The error variance is computed as the sum of the residual variance and the
#' rater variance. Accordingly, the rater variance is part of the error variance.
#' The standard #' error of measurement is the square root of this error variance.
#' The confidence intervals are computed with the exact F method. F = (k *
#' subject variance + error variance)/ error variance, with df1 = n - 1 and
#' df2 = n * (k - 1) (Shrout & Fleiss, 1979).
#' @author Iris Eekhout
#' @references
#' Shrout, P.E. & Fleiss, J.L. (1979) Intraclass Correlations: Uses in Assessing
#' Rater Reliability. Psychological Bulletin, 87(2), 420-428.
icc_oneway <- function(data, cols = colnames(data), alpha = 0.05, twoway = FALSE){

  k <- length(cols)
  n <- nrow(data)

  data1 <- data.frame(data) %>%
    mutate(level1 = 1:nrow(data)) %>% #add id column
    pivot_longer(cols = cols, names_to = "level2", values_to = "score")

  if(twoway){
  vc <- varcomp(score ~ (1|level1) + (1|level2), data1)
  #oneway from two-level model
  varpat_oneway <- ((k * vc["level1","vcov"]) - vc["level2","vcov"]) / k
  varerr_oneway <- (vc["level2","vcov"] + vc["Residual","vcov"])
  }

  if(!twoway){
  vc <- varcomp(formula = score ~ (1|level1), data1)
  #oneway from one-level model
  varpat_oneway <- vc["level1","vcov"]
  varerr_oneway <- vc["Residual","vcov"]
  }

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
    data.frame(
    list(
      icc = icc_o,
      L_icc = L_o,
      U_icc = U_o,
      sem = sem_o,
      varj_oneway = varpat_oneway,
      varerr_oneway = varerr_oneway
    )
    )
  )
}

#for wide data
icc_oneway2 <- function(data, cols = colnames(data), alpha = 0.05){

  k <- length(cols)
  n <- nrow(data)

  data1 <- data.frame(data) %>%
    mutate(level1 = 1:nrow(data)) %>% #add id column
    pivot_longer(cols = cols, names_to = "level2", values_to = "score")

  vc <- varcomp(formula = score ~ (1|level1), data1)

  #oneway
  varpat_oneway <- vc["level1","vcov"]
  varerr_oneway <- vc["Residual","vcov"]
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
      varj_oneway = varpat_oneway,
      varerr_oneway = varerr_oneway,
      icc = icc_o,
      sem = sem_o,
      L_icc = L_o,
      U_icc = U_o
    )
  )
}
