#' ICC agreement
#'
#' The intraclass correlations (ICC) of agreement for rater reliability using
#' the variance estimates from a linear mixed model. The function returns the
#' icc, standard error of measurment (sem) and confidence intervals for icc.
#'
#' @param model merMod object result from `icc_model()`.
#' @param alpha confidence interval level, default `alpha = 0.05`.
#' @importFrom lme4 ngrps VarCorr
#' @importFrom stats qf
#'
#' @return list
#' @export
#' @details
#' The icc type agreement is the variance between the subjects divided by the sum
#' of the subject variance, rater variance and the residual variance. The ICC for
#' agreement generalizes to other raters within a population (Shrout & Fleiss,
#' 1979). The `icc_model()` function is used to compute the variances.
#' This is a `lmer` model with a random slope for the subjects as well as for
#' the raters. The sem is the square root of the sum of the rater variance and
#' the error variance.
#' The confidence intervals are approximated to account for the three independent
#' variance components, as defined by Satterthwaite (1946) & Fleiss and Shrout (1978).
#' @author Iris Eekhout
#' @references
#' Fleiss, J. L., & Shrout, P. E. Approximate interval estimation for a certain
#' intraclass correlation coefficient. Psychometrika, 1978, 43, 259-262.
#' Satterthwaite, F. E. An approximate distribution of estimates of variance
#' components. Biometrics, 1946, 2, 110-114.
#' Shrout, P.E. & Fleiss, J.L. (1979) Intraclass Correlations: Uses in Assessing
#' Rater Reliability. Psychological Bulletin, 87(2), 420-428.
icc_agreement <- function(model, alpha = 0.05){

  k <- ngrps(model)[2]
  n <- ngrps(model)[1]
  vc <- as.data.frame(VarCorr(model))

  #agreement
  varpat_agr <- vc[1,4]
  varobs_agr <- vc[2,4]
  varerr_agr <- vc[3,4]
  icc_a <- varpat_agr/(varpat_agr + varobs_agr + varerr_agr)
  sem_a <- sqrt(varobs_agr + varerr_agr)
  MSB <-  (k * varpat_agr + varerr_agr)
  MSBt <- 1 +(k - 1)*icc_a #test
  MSEt <- 1-icc_a #test
  #F_a1 <- (n * varobs_agr + varerr_agr)/varerr_agr
  #JMS <- #changed after Shrout and Fleis check
  #EMS <- #changed after Shrout and Fleis check

  Fa1 <- MSBt/MSEt #test
  Fl <- qf(alpha/2, df1= (n-1) ,df2=n*(k-1))
  Fu <- qf(1-(alpha/2), df1=(n-1) , df2=n*(k-1))

  l_a <- (Fa1/Fu -1)/(Fa1/Fu+k-1) #test
  u_a <- (Fa1/Fl -1)/(Fa1/Fl+k-1) #test
  #where Fl and Fu are the ˛=2 and the 1  ˛=2th quantiles of the F -distribution    with degrees of freedom
  # N  1 and N.k  1/, respectively.


  ## following Shrout & Fleis 1979 - Fleiss & Shrout 1978
  F_a1 <- (n * varobs_agr + varerr_agr)/(varerr_agr) #Shrout and Fleis check === not sure on the first part (RMS)

  vn <- (k - 1) * (n - 1) * (k * icc_a * F_a1 + n * (1 + (k - 1) * icc_a) - k * icc_a)^2 #checked with shrout fleis.
  vd <- (n - 1) * k^2 * icc_a^2 * F_a1^2 + (n * (1 + (k - 1) * icc_a) - k * icc_a)^2 #checked with shrout fleis.
  v <- vn/vd #checked with shrout fleis.
  F3U <- qf(1 - alpha/2, n - 1, v)#Shrout and fleis checked
  F3L <- qf(1 - alpha/2, v, n - 1)##checked with shrout fleis.
  L_a <- (n * (MSB - F3U * varerr_agr))/
    (F3U * (k * (n * varobs_agr + varerr_agr) + (k * n - k - n) * varerr_agr) + n * MSB)##checked with shrout fleis.

  U_a <- (n * (F3L * MSB - varerr_agr))/(k * (n * varobs_agr + varerr_agr) + (k * n -
                                                                              k - n) * varerr_agr + n * F3L * MSB)#checked with shrout fleis.

  return(
    list(
      varj_agr = varpat_agr,
      varr_agr = varobs_agr,
      varerr_agr = varerr_agr,
      icc = icc_a,
      sem = sem_a,
      L_icc = l_a, #use upper case L for PSYCH version/ lowercase l for same logic as in oneway and consistency - however the observer part is then ignored.
      U_icc = u_a #use upper case U for PSYCH version/ lowercase l for same logic as in oneway and consistency - however the observer part is then ignored.
    )
  )
}
