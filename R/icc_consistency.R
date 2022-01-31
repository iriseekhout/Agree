#' ICC consistency
#'
#' The intraclass correlations (ICC) of consistency for rater reliability using
#' the variance estimates from a linear mixed model. The function returns the
#' ICC, standard error of measurment (sem) and confidence intervals for ICC.
#'
#' @param data data.frame with a column for each observer/rater and a row per
#' rated subject.
#' @param cols character vector with the column names to be used as observers.
#' Default is `cols = colnames(data)`.
#' @param alpha confidence interval level, default `alpha = 0.05`.
#' @param twoway logical indicator if the variance components are estimated from
#' the two-way model default: `twoway = FALSE`.
#' @importFrom lme4 ngrps VarCorr
#' @importFrom dplyr mutate %>%
#' @importFrom tidyr pivot_longer
#' @return list
#' @export
#' @details
#' The ICC type consistency is the variance between the subjects divided by the sum
#' of the subject variance and the residual variance. The subject variance and
#' error variance are adjusted for the fixed rater effect, accordingly the rater
#' variance is not used to calculate the ICC. The ICC for consistency generalizes
#' only to the fixed set of raters in the data (Shrout & Fleiss,
#' 1979). The `icc_model()` function is used to compute the variances.
#' This is a `lmer` model with a random slope for the subjects as well as for
#' the raters. The sem is the square root of the error variance.
#' The confidence are computed with the exact F method. F = (k * subject variance +
#' error variance)/ error variance, with df1 = n - 1 and df2 = (n - 1) * (k - 1)
#' (Shrout & Fleiss, 1979).
#' @author Iris Eekhout
#' @references
#' Fleiss, J. L., & Shrout, P. E. Approximate interval estimation for a certain
#' intraclass correlation coefficient. Psychometrika, 1978, 43, 259-262.
icc_consistency <- function(data, cols = colnames(data), alpha = 0.05, twoway = FALSE){

  k <- length(cols)
  n <- nrow(data)

  data1 <- data.frame(data) %>%
    mutate(level1 = 1:nrow(data)) %>% #add id column
    tidyr::pivot_longer(cols = cols, names_to = "level2", values_to = "score")

  if(twoway){
    vc <- varcomp(score ~ (1|level1) + (1|level2), data1)
    }

  if(!twoway){
    vc <- varcomp(score ~ (1|level1) + level2, data1)
    #oneway from one-level model
   }

    #consistency
  varpat_cons <- vc["level1","vcov"]
  varerr_cons <- vc["Residual","vcov"]

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
    data.frame(
    list(
      icc = icc_c,
      L_icc = L_c,
      U_icc = U_c,
      sem = sem_c,
      varj_cons = varpat_cons,
      varerr_cons = varerr_cons
      )
    )
  )
}


icc_consistency2 <- function(data, cols = colnames(data), alpha = 0.05){
  k <- length(cols)
  n <- nrow(data)
  data1 <- data.frame(data) %>%
    mutate(level1 = 1:nrow(data)) %>% #add id column
    pivot_longer(cols = cols, names_to = "level2", values_to = "score")
  model <- lmer(score ~ (1|level1) + level2 , data1, REML = T)


  vc <- as.data.frame(lme4::VarCorr(model))[,c("grp", "vcov")]
  rownames(vc) <- vc[,"grp"]

  #consistency
  varpat_cons <- vc["level1","vcov"]
  varerr_cons <- vc["Residual","vcov"]

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
      varj_cons = varpat_cons,
      varerr_cons = varerr_cons,
      icc = icc_c,
      sem = sem_c,
      L_icc = L_c,
      U_icc = U_c
    )
  )
}
