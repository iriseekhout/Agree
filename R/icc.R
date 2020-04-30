#' Intra class correlation for rater reliability
#'
#' @param data data.frame with repeated measures or observations in the columns and rated subjects in the rows
#' @param icc icc method \code{icc = c("oneway", "agreement", "consistency")}, see details for explanatory formulas.
#' @param cols column names used for the repeated measures, default \code{cols = colnames(data)}
#' @param sem logical vector if sem are returned.
#' @param confint logical vector if confidence interval for icc are computed (see details for formulas).
#' @param alpha the confidence level required.
#' @param var logical vector if variances are returned.
#' @importFrom dplyr %>% mutate
#' @importFrom tidyr pivot_longer
#' @importFrom lme4 lmer VarCorr
#' @importFrom stats qf qnorm quantile
#' @return matrix with relevant output
#' @export
#'
#' @examples
#' mam <- breast[,c("Mam1_totalscoreLikertscale","Mam2_totalscoreLikertscale","Mam3_totalscoreLikertscale")]
#' icc(data = mam, confint = TRUE, var = TRUE)
#' pch <- breast[,c("PCH1_totalscoreLikertscale", "PCH2_totalscoreLikertscale","PCH3_totalscoreLikertscale","PCH4_totalscoreLikertscale","PCH5_totalscoreLikertscale")]
#' icc(data = pch)
#' icc(data = pch, confint = FALSE, var = TRUE)
icc <- function(data,
                method = c("oneway", "agreement", "consistency"),
                cols = colnames(data),
                sem = TRUE,
                confint = TRUE,
                alpha = 0.05,
                var = FALSE){

  k <- ncol(data)
  n <- nrow(data)
  data1 <- data.frame(data) %>%
    mutate(id = 1:nrow(data)) %>% #add id column
    pivot_longer(cols = cols, names_to = "observer", values_to = "score")

  ICC <- matrix(NA, nrow = 3, ncol = 7)
  rownames(ICC) <- c("oneway", "agreement", "consistency")
  colnames(ICC) <- c("icc", "lower", "upper", "sem", "varpat", "varobs", "varerr")

  REML_model <- lmer(score ~ (1|id) + (1|observer), data1, REML = T)
  vc <- as.data.frame(lme4::VarCorr(REML_model))

  if("oneway" %in% method){
  nom <- ((k * vc[1,4]+vc[3,4]) - (vc[3,4]+vc[2,4]))
  varpat_oneway <- nom/k
  denom <- ((k * vc[1,4]+vc[3,4]) + (k-1) * (vc[3,4]+vc[2,4]) )
  varerr_oneway <- (denom - nom)/k

  ICC["oneway", "varpat"] <- varpat_oneway
  ICC["oneway", "varerr"] <- varerr_oneway

  icc_o <- varpat_oneway / (varpat_oneway + varerr_oneway)

  # compute SEM one way
  sem_o <- sqrt(varerr_oneway)
  ICC["oneway", "icc"] <- icc_o
  ICC["oneway", "sem"] <- sem_o

  #ci (from psych package)
  F_o <- (k * varpat_oneway + varerr_oneway)/varerr_oneway
  dfon <- n - 1
  dfod <- n * (k - 1)
  F_oL <- F_o/qf(1 - alpha/2, dfon, dfod) #or alpha/2?not dividing by 2 is shrout fleis
  F_oU <- F_o * qf(1 - alpha/2, dfod, dfon) #or alpha/2?
  L_o <- (F_oL - 1)/(F_oL + (k - 1))
  U_o <- (F_oU - 1)/(F_oU + k - 1)
  ICC["oneway", "lower"] <- L_o
  ICC["oneway", "upper"] <- U_o
  }

  if("agreement" %in% method){
  # variance components
  varpat_agr <- vc[1,4]
  varobs_agr <- vc[2,4]
  varerr_agr <- vc[3,4]

  ICC["agreement", "varpat"] <- varpat_agr
  ICC["agreement", "varerr"] <- varerr_agr
  ICC["agreement", "varobs"] <- varobs_agr

  # compute ICC agreement: ICC 2,1
  icc_a <- varpat_agr/(varpat_agr + varobs_agr + varerr_agr)
  # compute SEM agreement
  sem_a <- sqrt(varobs_agr + varerr_agr)
  ICC["agreement", "icc"] <- icc_a
  ICC["agreement", "sem"] <- sem_a

  MSB <-  (k * varpat_agr + varerr_agr)
  F_a1 <- (n * varobs_agr + varerr_agr)/varerr_agr

  vn <- (k - 1) * (n - 1) * ((k * icc_a * F_a1 + n *
                                     (1 + (k - 1) * icc_a) - k * icc_a))^2
  vd <- (n - 1) * k^2 * icc_a^2 * F_a1^2 + (n * (1 + (k - 1) * icc_a) - k * icc_a)^2
  v <- vn/vd
  F3U <- qf(1 - alpha/2, n - 1, v)#or alpha/2?not dividing by 2 is shrout fleis
  F3L <- qf(1 - alpha/2, v, n - 1)#or alpha/2?
  L3 <- n * (MSB - F3U * varerr_agr)/(F3U * (k * (n * varobs_agr + varerr_agr) + (k *
                                                        n - k - n) * varerr_agr) + n * MSB)#

  U3 <- n * (F3L * MSB - varerr_agr)/(k * (n * varobs_agr + varerr_agr) + (k * n -
                                                 k - n) * varerr_agr + n * F3L * MSB)


  ICC["agreement", "lower"] <- L3
  ICC["agreement", "upper"] <- U3


  }

  if("consistency" %in% method){
  # Two way consistency: ICC 3,1  (fixed; consistency)
  #REMLmodel_cons <- lmer(score ~ observer + (1|id), data1, REML = T) # two way consistency
# variance components
  varpat_cons <- vc[1,4]
  varerr_cons <- vc[3,4]

  ICC["consistency", "varpat"] <- varpat_cons
  ICC["consistency", "varerr"] <- varerr_cons

  # compute ICC consistency: ICC 3,1
  icc_c <- varpat_cons/(varpat_cons + varerr_cons)
  # compute SEM consistency
  sem_c <- sqrt(varerr_cons)
  ICC["consistency", "icc"] <- icc_c
  ICC["consistency", "sem"] <- sem_c

  F_c <- (k * varpat_cons + varerr_cons)/varerr_cons
  df21n <- n - 1
  df21d <- (n - 1) * (k - 1)
  F3L <- F_c/qf(1 - alpha/2, df21n, df21d) #or alpha/2? not dividing by 2 is shrout fleis
  F3U <- F_c * qf(1 - alpha/2, df21d, df21n)#or alpha/2?
  L_c <- (F3L - 1)/(F3L + k - 1)
  U_c <- (F3U - 1)/(F3U + k - 1)
  ICC["consistency", "lower"] <- L_c
  ICC["consistency", "upper"] <- U_c
  }



  if(confint == FALSE & sem == TRUE & var == FALSE){
    return(ICC[method, c("icc", "sem")])
  }
  if(confint == FALSE & sem == FALSE & var == FALSE){
    return(ICC[method, c("icc")])
  }
  if(confint == TRUE & sem == TRUE & var == FALSE){
    return(ICC[method, c("icc", "lower", "upper", "sem")])
  }

  if(confint == TRUE & sem == FALSE & var == FALSE){
    return(ICC[method, c("icc",  "lower", "upper")])
  }
  if(confint == FALSE & sem == TRUE & var == TRUE){
    return(ICC[method, c("icc", "sem", "varpat", "varobs", "varerr")])
  }
  if(confint == FALSE & sem == FALSE & var == TRUE){
    return(ICC[method, c("icc", "varpat", "varobs", "varerr")])
  }
  if(confint == TRUE & sem == TRUE & var == TRUE){
    return(ICC[method, c("icc", "lower", "upper", "sem", "varpat", "varobs", "varerr")])
  }

  if(confint == TRUE & sem == FALSE & var == TRUE){
    return(ICC[method, c("icc",  "lower", "upper", "varpat", "varobs", "varerr")])
  }

}
