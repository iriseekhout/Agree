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
  colnames(ICC) <- c("icc", "icc_low", "icc_high", "sem", "varpat", "varobs", "varerr")

  if("oneway" %in% method){
  REMLmodel_oneway <- lmer(score ~ (1|id), data=data1, REML = T) # one way

  # variance components
  varpat_oneway <- as.data.frame(VarCorr(REMLmodel_oneway))[1,4] #deze valt veel lager uit dan de varpat in beide andere modellen.
  varerr_oneway <- as.data.frame(VarCorr(REMLmodel_oneway))[2,4] #bevat error en obs (= varobs_agr + varerr_agr)
  ICC["oneway", "varpat"] <- varpat_oneway
  ICC["oneway", "varerr"] <- varerr_oneway

  # compute ICC one way: ICC 1,1
  icc_o <- varpat_oneway/(varpat_oneway + varerr_oneway)
  # compute SEM one way
  sem_o <- sqrt(varerr_oneway)
  ICC["oneway", "icc"] <- icc_o
  ICC["oneway", "sem"] <- sem_o

  #ci (from psych package)
  F_o <- (k * varpat_oneway + varerr_oneway)/varerr_oneway
  dfon <- n - 1
  dfod <- n * (k - 1)
  F_oL <- F_o/qf(1 - alpha/2, dfon, dfod)
  F_oU <- F_o * qf(1 - alpha/2, dfod, dfon)
  L_o <- (F_oL - 1)/(F_oL + (k - 1))
  U_o <- (F_oU - 1)/(F_oU + k - 1)
  ICC["oneway", "icc_low"] <- L_o
  ICC["oneway", "icc_high"] <- U_o
  }

  if("agreement" %in% method){
  # Two way agreement: ICC 2,1 (random; agreement)
  REMLmodel_agr <- lmer(score ~ (1|id) + (1|observer), data1, REML = T) # two way agreement
  # variance components
  varpat_agr <- as.data.frame(VarCorr(REMLmodel_agr))[1,4]
  varobs_agr <- as.data.frame(VarCorr(REMLmodel_agr))[2,4] #is 0 als er geen variatie tussen raters is.
  varerr_agr <- as.data.frame(VarCorr(REMLmodel_agr))[3,4]
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
  F3U <- qf(1 - alpha/2, n - 1, v)
  F3L <- qf(1 - alpha/2, v, n - 1)
  L3 <- n * (MSB - F3U * varerr_agr)/(F3U * (k * (n * varobs_agr + varerr_agr) + (k *
                                                        n - k - n) * varerr_agr) + n * MSB)#

  U3 <- n * (F3L * MSB - varerr_agr)/(k * (n * varobs_agr + varerr_agr) + (k * n -
                                                 k - n) * varerr_agr + n * F3L * MSB)


  ICC["agreement", "icc_low"] <- L3
  ICC["agreement", "icc_high"] <- U3


  }

  if("consistency" %in% method){
  # Two way consistency: ICC 3,1  (fixed; consistency)
  REMLmodel_cons <- lmer(score ~ observer + (1|id), data1, REML = T) # two way consistency

  # variance components
  varpat_cons <- as.data.frame(VarCorr(REMLmodel_cons))[1,4]
  varerr_cons <- as.data.frame(VarCorr(REMLmodel_cons))[2,4]
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
  F3L <- F_c/qf(1 - alpha/2, df21n, df21d)
  F3U <- F_c * qf(1 - alpha/2, df21d, df21n)
  L_c <- (F3L - 1)/(F3L + k - 1)
  U_c <- (F3U - 1)/(F3U + k - 1)
  ICC["consistency", "icc_low"] <- L_c
  ICC["consistency", "icc_high"] <- U_c
  }



  if(confint == FALSE & sem == TRUE & var == FALSE){
    return(ICC[method, c("icc", "sem")])
  }
  if(confint == FALSE & sem == FALSE & var == FALSE){
    return(ICC[method, c("icc")])
  }
  if(confint == TRUE & sem == TRUE & var == FALSE){
    return(ICC[method, c("icc", "icc_low", "icc_high", "sem")])
  }

  if(confint == TRUE & sem == FALSE & var == FALSE){
    return(ICC[method, c("icc",  "icc_low", "icc_high")])
  }
  if(confint == FALSE & sem == TRUE & var == TRUE){
    return(ICC[method, c("icc", "sem", "varpat", "varobs", "varerr")])
  }
  if(confint == FALSE & sem == FALSE & var == TRUE){
    return(ICC[method, c("icc", "varpat", "varobs", "varerr")])
  }
  if(confint == TRUE & sem == TRUE & var == TRUE){
    return(ICC[method, c("icc", "icc_low", "icc_high", "sem", "varpat", "varobs", "varerr")])
  }

  if(confint == TRUE & sem == FALSE & var == TRUE){
    return(ICC[method, c("icc",  "icc_low", "icc_high", "varpat", "varobs", "varerr")])
  }

}
