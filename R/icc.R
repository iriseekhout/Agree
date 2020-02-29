#' Intra class correlation for rater reliability
#'
#' @param data data.frame with repeated measures or observations in the columns and rated subjects in the rows
#' @param icc icc method \code{icc = c("oneway", "agreement", "consistency")}, see details for explanatory formulas.
#' @param cols column names used for the repeated measures, default \code{cols = colnames(data)}
#' @param sem logical vector if sem should be returned
#' @param confint logical vector if confidence interval for icc should be computed (see details for formulas)
#' @param alpha the confidence level required.
#' @importFrom dplyr %>% mutate
#' @importFrom tidyr pivot_longer
#' @importFrom lme4 lmer VarCorr
#' @importFrom stats qf qnorm quantile
#' @return matrix with relevant output
#' @export
#'
#' @examples
#' means <- c(1, rep(0,(5-1)))
#' means <- c(rep(0, 5))
#' cov <- matrix(0.7,5,5)
#' diag(cov) <- 1 # large variance
#' cov <- cov*1 # adjust variance to small, medium or large
#' dat <- MASS::mvrnorm(means, cov, n=100)
#' dat <- as.data.frame(dat)
#' icc(dat, confint = TRUE)
icc <- function(data,
                method = c("oneway", "agreement", "consistency"),
                cols = colnames(data),
                sem = TRUE,
                confint = TRUE,
                alpha = 0.05){

  k <- ncol(data)
  n <- nrow(data)
  data1 <- data.frame(data) %>%
    mutate(id = 1:nrow(data)) %>% #add id column
    pivot_longer(cols = cols, names_to = "observer", values_to = "score")

  ICC <- matrix(NA, nrow = 3, ncol = 4)
  rownames(ICC) <- c("oneway", "agreement", "consistency")
  colnames(ICC) <- c("icc", "icc_low", "icc_high", "sem")

  if("oneway" %in% method){
  REMLmodel_oneway <- lmer(score ~ (1|id), data=data1, REML = T) # one way

  # variance components
  varpat_oneway <- as.data.frame(VarCorr(REMLmodel_oneway))[1,4] #deze valt veel lager uit dan de varpat in beide andere modellen.
  varerr_oneway <- as.data.frame(VarCorr(REMLmodel_oneway))[2,4] #bevat error en obs (= varobs_agr + varerr_agr)


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
  vd <- (n - 1) * k^2 * icc_a^2 * F_a1^2 + (n * (1 +
                                                         (k - 1) * icc_a) - k * icc_a)^2
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



  if(confint == FALSE & sem == TRUE){
    return(ICC[method, c("icc", "sem")])
  }
  if(confint == FALSE & sem == FALSE){
    return(ICC[method, c("icc")])
  }
  if(confint == TRUE & sem == TRUE){
    return(ICC[method, c("icc", "icc_low", "icc_high", "sem")])
  }

  if(confint == TRUE & sem == FALSE){
    return(ICC[method, c("icc",  "icc_low", "icc_high")])
  }

}
