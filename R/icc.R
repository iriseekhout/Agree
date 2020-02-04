#' Intra class correlation for rater reliability
#'
#' @param data data.frame with repeated measures or observations in the columns and rated subjects in the rows
#' @param icc icc method \code{icc = c("oneway", "agreement", "consistency")}, see details for explanatory formulas.
#' @param cols column names used for the repeated measures, default \code{cols = colnames(data)}
#' @param sem logical vector if sem should be returned
#' @param confint logical vector if confidence interval for icc should be computed (see details for formulas)
#' @param level the confidence level required.
#' @importFrom dplyr %>%
#' @importFrom tidyr pivot_longer
#' @importFrom lme4 lmer
#' @return matrix with relevant output
#' @export
#'
#' @examples
#' icc <- 2
#' means <- c(1, rep(0,(5-1)))
#' means <- c(rep(0, 5))
#' means  <- means * 1
#' cov <- matrix(0.7,5,5)
#' diag(cov) <- 1 # large variance
#' cov <- cov*1 # adjust variance to small, medium or large
#' dat <- MASS::mvrnorm(means, cov, n=100)
#' dat <- as.data.frame(dat)
#' icc(dat)
icc <- function(data,
                method = c("oneway", "agreement", "consistency"),
                cols = colnames(data),
                sem = TRUE,
                confint = FALSE,
                level = 0.95){

  data <- data.frame(data) %>%
    mutate(id = 1:nrow(data)) %>% #add id column
    pivot_longer(cols = cols, names_to = "observer", values_to = "score")

  ICC <- matrix(NA, nrow = 3, ncol = 4)
  rownames(ICC) <- c("oneway", "agreement", "consistency")
  colnames(ICC) <- c("icc", "icc_low", "icc_high", "sem")

  if("oneway" %in% method){
  REMLmodel_oneway <- lmer(score ~ (1|id), data, REML = T) # one way

  # variance components
  varpat_oneway <- as.data.frame(VarCorr(REMLmodel_oneway))[1,4] #deze valt veel lager uit dan de varpat in beide andere modellen.
  varerr_oneway <- as.data.frame(VarCorr(REMLmodel_oneway))[2,4] #bevat error en obs (= varobs_agr + varerr_agr)


  # compute ICC one way: ICC 1,1
  icc_o <- varpat_oneway/(varpat_oneway + varerr_oneway)
  # compute SEM one way
  sem_o <- sqrt(varerr_oneway)
  ICC["oneway", "icc"] <- icc_o
  ICC["oneway", "sem"] <- sem_o
  }

  if("agreement" %in% method){
  # Two way agreement: ICC 2,1 (random; agreement)
  REMLmodel_agr <- lmer(score ~ (1|id) + (1|observer), data, REML = T) # two way agreement
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
  }

  if("consistency" %in% method){
  # Two way consistency: ICC 3,1  (fixed; consistency)
  REMLmodel_cons <- lmer(score ~ observer + (1|id), data, REML = T) # two way consistency

  # variance components
  varpat_cons <- as.data.frame(VarCorr(REMLmodel_cons))[1,4]
  varerr_cons <- as.data.frame(VarCorr(REMLmodel_cons))[2,4]

  # compute ICC consistency: ICC 3,1
  icc_c <- varpat_cons/(varpat_cons + varerr_cons)
  # compute SEM consistency
  sem_c <- sqrt(varerr_cons)
  ICC["consistency", "icc"] <- icc_c
  ICC["consistency", "sem"] <- sem_c
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
