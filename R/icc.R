#' Intra class correlation for rater reliability
#'
#' @param data data.frame with repeated measures or observations in the columns
#'   and rated subjects in the rows
#' @param method type of ICC that is returned, options are:
#' `c("oneway", "agreement", "consistency")`, the default returns all.
#' @param format character string indicating the data structure as either "wide"
#' or "long". Default `format = "wide"`.
#' @param cols column names used for the repeated measures in the wide format,
#' default uses `cols = colnames(data)`
#' @param levels character vector for the column names used for the levels.
#' Default is `levels = c("id", "rater")`.
#' @param sem logical vector if standard error of measurement is returned.
#' @param confint logical vector if confidence interval for ICC is returned.
#' @param alpha the confidence level required, default `alpha = 0.05`.
#' @param var logical vector if variance estimates are returned.
#' @return matrix with relevant output
#' @export
#' @seealso [icc_agreement()] [icc_oneway()] [icc_consistency()]
#' @examples
#' mam <-
#' breast[,c("Mam1_score","Mam2_score", "Mam3_score")]
#' icc(data = mam, confint = TRUE, var = TRUE)
#' pch <- breast[,c("PCH1_score", "PCH2_score","PCH3_score",
#' "PCH4_score","PCH5_score")]
#' icc(data = pch)
#' icc(data = pch, confint = FALSE, var = TRUE)
icc <- function(data,
                method = c("oneway", "agreement", "consistency"),
                format = "wide",
                cols = colnames(data),
                levels = c("id", "rater"),
                sem = TRUE,
                confint = TRUE,
                alpha = 0.05,
                var = FALSE){



  if(format == "wide"){
  data <- data.frame(data) %>%
    mutate(id = 1:nrow(data)) %>% #add id column
    pivot_longer(cols = cols, names_to = "rater", values_to = "score")
  levels <- c("id", "rater")
  }
  model <- icc_model2(data = data, levels = levels)

  #model <- icc_model(data = data, cols = cols)

  #output table definition based on model
  vc <- data.frame(VarCorr(model))
  ICC <- matrix(NA, nrow = length(method), ncol = (4 + nrow(vc)))
  rownames(ICC) <- method
  colnames(ICC) <- c("icc", "lower", "upper", "sem", paste("var", vc$grp, sep = "_"))


  if("oneway" %in% method){

  icc_ow <- icc_oneway(model, alpha = alpha)
  ICC["oneway", paste("var", vc$grp[1], sep = "_")] <- icc_ow$varj_oneway
  ICC["oneway", paste("var", vc$grp[3], sep = "_")] <- icc_ow$varerr_oneway

  #icc_o <- varj_oneway / (varj_oneway + varerr_oneway)

  # compute SEM one way
  #sem_o <- sqrt(varerr_oneway)
  ICC["oneway", "icc"] <- icc_ow$icc
  ICC["oneway", "sem"] <- icc_ow$sem

  #ci (from psych package)
  #F_o <- (k * varj_oneway + varerr_oneway)/varerr_oneway
  #dfon <- n - 1
  #dfod <- n * (k - 1)
  #F_oL <- F_o/qf(1 - alpha/2, dfon, dfod) #or alpha/2?not dividing by 2 is shrout fleis
  #F_oU <- F_o * qf(1 - alpha/2, dfod, dfon) #or alpha/2?
  #L_icc <- (F_oL - 1)/(F_oL + (k - 1))
  #U_icc <- (F_oU - 1)/(F_oU + k - 1)
  ICC["oneway", "lower"] <- icc_ow$L_icc
  ICC["oneway", "upper"] <- icc_ow$U_icc
  }

  if("agreement" %in% method){

    icc_am <- icc_agreement(model, alpha = alpha)

  # variance components
  #varj_agr <- vc[1,4]
  #varr_agr <- vc[2,4]
  #varerr_agr <- vc[3,4]

  ICC["agreement", paste("var", vc$grp[1], sep = "_")] <- icc_am$varj_agr
  ICC["agreement", paste("var", vc$grp[3], sep = "_")] <- icc_am$varerr_agr
  ICC["agreement", paste("var", vc$grp[2], sep = "_")] <- icc_am$varr_agr

  # compute ICC agreement: ICC 2,1
  #icc_a <- varj_agr/(varj_agr + varr_agr + varerr_agr)
  # compute SEM agreement
  #sem_a <- sqrt(varr_agr + varerr_agr)
  ICC["agreement", "icc"] <- icc_am$icc
  ICC["agreement", "sem"] <- icc_am$sem

  #MSB <-  (k * varj_agr + varerr_agr)
  #F_a1 <- (n * varr_agr + varerr_agr)/varerr_agr

  #vn <- (k - 1) * (n - 1) * ((k * icc_a * F_a1 + n *
  #                                   (1 + (k - 1) * icc_a) - k * icc_a))^2
  #vd <- (n - 1) * k^2 * icc_a^2 * F_a1^2 + (n * (1 + (k - 1) * icc_a) - k * icc_a)^2
  #v <- vn/vd
  #F3U <- qf(1 - alpha/2, n - 1, v)#or alpha/2?not dividing by 2 is shrout fleis
  #F3L <- qf(1 - alpha/2, v, n - 1)#or alpha/2?
  #L3 <- n * (MSB - F3U * varerr_agr)/(F3U * (k * (n * varr_agr + varerr_agr) + (k *
  #                                                      n - k - n) * varerr_agr) + n * MSB)#
  #U3 <- n * (F3L * MSB - varerr_agr)/(k * (n * varr_agr + varerr_agr) + (k * n -
  #                                               k - n) * varerr_agr + n * F3L * MSB)

  ICC["agreement", "lower"] <- icc_am$L_icc
  ICC["agreement", "upper"] <- icc_am$U_icc
  #}
 # if(boot){
#    icc_a_boot <- function(data,x) {
#      icc_agreement(model = icc_model(data[x,],cols = cols))$icc_a}
#    res1a <- boot::boot(data,icc_a_boot, R = b)
#    BCI_a <-  quantile(res1a$t,c(alpha/2,(1-alpha/2)), na.rm=TRUE)
#    L_icc <- BCI_a[1]
#    U_icc <- BCI_a[2]

  #  ICC["agreement", "lower"] <- L_icc
  #  ICC["agreement", "upper"] <- U_icc

  #}

  }

  if("consistency" %in% method){

    icc_cs <- icc_consistency(model, alpha = alpha)
  # Two way consistency: ICC 3,1  (fixed; consistency)
  #REMLmodeL_iccons <- lmer(score ~ observer + (1|id), data1, REML = T) # two way consistency
# variance components
  #varj_cons <- vc[1,4]
  #varerr_cons <- vc[3,4]

  ICC["consistency", paste("var", vc$grp[1], sep = "_")] <- icc_cs$varj_cons
  ICC["consistency", paste("var", vc$grp[3], sep = "_")] <- icc_cs$varerr_cons

  # compute ICC consistency: ICC 3,1
  #icc_c <- varj_cons/(varj_cons + varerr_cons)
  # compute SEM consistency
  #sem_c <- sqrt(varerr_cons)
  ICC["consistency", "icc"] <- icc_cs$icc
  ICC["consistency", "sem"] <- icc_cs$sem

  #F_c <- (k * varj_cons + varerr_cons)/varerr_cons
  #df21n <- n - 1
  #df21d <- (n - 1) * (k - 1)
  #F3L <- F_c/qf(1 - alpha/2, df21n, df21d) #or alpha/2? not dividing by 2 is shrout fleis
  #F3U <- F_c * qf(1 - alpha/2, df21d, df21n)#or alpha/2?
  #L_icc <- (F3L - 1)/(F3L + k - 1)
  #U_icc <- (F3U - 1)/(F3U + k - 1)
  ICC["consistency", "lower"] <- icc_cs$L_icc
  ICC["consistency", "upper"] <- icc_cs$U_icc
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
    return(ICC[method, c("icc", "sem", paste("var", vc$grp, sep = "_"))])
  }
  if(confint == FALSE & sem == FALSE & var == TRUE){
    return(ICC[method, c("icc", paste("var", vc$grp, sep = "_"))])
  }
  if(confint == TRUE & sem == TRUE & var == TRUE){
    return(ICC[method, ])
  }

  if(confint == TRUE & sem == FALSE & var == TRUE){
    return(ICC[method, c("icc",  "lower", "upper", paste("var", vc$grp, sep = "_"))])
  }

}



icc2 <- function(data,
                method = c("oneway", "agreement", "consistency"),
                #format = "wide",
                cols = colnames(data),
                #levels = c("id", "rater"),
                sem = TRUE,
                confint = TRUE,
                alpha = 0.05,
                var = FALSE){



  #if(format == "wide"){
  #  data <- data.frame(data) %>%
  #    mutate(id = 1:nrow(data)) %>% #add id column
  #    pivot_longer(cols = cols, names_to = "rater", values_to = "score")
  #  levels <- c("id", "rater")
  #}
  #model <- icc_model2(data = data, levels = levels)

  model <- icc_model(data = data, cols = cols) #for icc agreement (unchanged)

  #output table definition based on model
  vc <- data.frame(VarCorr(model))
  ICC <- matrix(NA, nrow = length(method), ncol = (4 + nrow(vc)))
  rownames(ICC) <- method
  colnames(ICC) <- c("icc", "lower", "upper", "sem", paste("var", vc$grp, sep = "_"))


  if("oneway" %in% method){

    icc_ow <- icc_oneway2(data = data, cols = cols, alpha = alpha)
    ICC["oneway", paste("var", vc$grp[1], sep = "_")] <- icc_ow$varj_oneway
    ICC["oneway", paste("var", vc$grp[3], sep = "_")] <- icc_ow$varerr_oneway

    #icc_o <- varj_oneway / (varj_oneway + varerr_oneway)

    # compute SEM one way
    #sem_o <- sqrt(varerr_oneway)
    ICC["oneway", "icc"] <- icc_ow$icc
    ICC["oneway", "sem"] <- icc_ow$sem

    #ci (from psych package)
    #F_o <- (k * varj_oneway + varerr_oneway)/varerr_oneway
    #dfon <- n - 1
    #dfod <- n * (k - 1)
    #F_oL <- F_o/qf(1 - alpha/2, dfon, dfod) #or alpha/2?not dividing by 2 is shrout fleis
    #F_oU <- F_o * qf(1 - alpha/2, dfod, dfon) #or alpha/2?
    #L_icc <- (F_oL - 1)/(F_oL + (k - 1))
    #U_icc <- (F_oU - 1)/(F_oU + k - 1)
    ICC["oneway", "lower"] <- icc_ow$L_icc
    ICC["oneway", "upper"] <- icc_ow$U_icc
  }

  if("agreement" %in% method){

    icc_am <- icc_agreement(model, alpha = alpha)

    # variance components
    #varj_agr <- vc[1,4]
    #varr_agr <- vc[2,4]
    #varerr_agr <- vc[3,4]

    ICC["agreement", paste("var", vc$grp[1], sep = "_")] <- icc_am$varj_agr
    ICC["agreement", paste("var", vc$grp[3], sep = "_")] <- icc_am$varerr_agr
    ICC["agreement", paste("var", vc$grp[2], sep = "_")] <- icc_am$varr_agr

    # compute ICC agreement: ICC 2,1
    #icc_a <- varj_agr/(varj_agr + varr_agr + varerr_agr)
    # compute SEM agreement
    #sem_a <- sqrt(varr_agr + varerr_agr)
    ICC["agreement", "icc"] <- icc_am$icc
    ICC["agreement", "sem"] <- icc_am$sem

    #MSB <-  (k * varj_agr + varerr_agr)
    #F_a1 <- (n * varr_agr + varerr_agr)/varerr_agr

    #vn <- (k - 1) * (n - 1) * ((k * icc_a * F_a1 + n *
    #                                   (1 + (k - 1) * icc_a) - k * icc_a))^2
    #vd <- (n - 1) * k^2 * icc_a^2 * F_a1^2 + (n * (1 + (k - 1) * icc_a) - k * icc_a)^2
    #v <- vn/vd
    #F3U <- qf(1 - alpha/2, n - 1, v)#or alpha/2?not dividing by 2 is shrout fleis
    #F3L <- qf(1 - alpha/2, v, n - 1)#or alpha/2?
    #L3 <- n * (MSB - F3U * varerr_agr)/(F3U * (k * (n * varr_agr + varerr_agr) + (k *
    #                                                      n - k - n) * varerr_agr) + n * MSB)#
    #U3 <- n * (F3L * MSB - varerr_agr)/(k * (n * varr_agr + varerr_agr) + (k * n -
    #                                               k - n) * varerr_agr + n * F3L * MSB)

    ICC["agreement", "lower"] <- icc_am$L_icc
    ICC["agreement", "upper"] <- icc_am$U_icc
    #}
    # if(boot){
    #    icc_a_boot <- function(data,x) {
    #      icc_agreement(model = icc_model(data[x,],cols = cols))$icc_a}
    #    res1a <- boot::boot(data,icc_a_boot, R = b)
    #    BCI_a <-  quantile(res1a$t,c(alpha/2,(1-alpha/2)), na.rm=TRUE)
    #    L_icc <- BCI_a[1]
    #    U_icc <- BCI_a[2]

    #  ICC["agreement", "lower"] <- L_icc
    #  ICC["agreement", "upper"] <- U_icc

    #}

  }

  if("consistency" %in% method){

    icc_cs <- icc_consistency2(data = data, cols = cols, alpha = alpha)
    # Two way consistency: ICC 3,1  (fixed; consistency)
    #REMLmodeL_iccons <- lmer(score ~ observer + (1|id), data1, REML = T) # two way consistency
    # variance components
    #varj_cons <- vc[1,4]
    #varerr_cons <- vc[3,4]

    ICC["consistency", paste("var", vc$grp[1], sep = "_")] <- icc_cs$varj_cons
    ICC["consistency", paste("var", vc$grp[3], sep = "_")] <- icc_cs$varerr_cons

    # compute ICC consistency: ICC 3,1
    #icc_c <- varj_cons/(varj_cons + varerr_cons)
    # compute SEM consistency
    #sem_c <- sqrt(varerr_cons)
    ICC["consistency", "icc"] <- icc_cs$icc
    ICC["consistency", "sem"] <- icc_cs$sem

    #F_c <- (k * varj_cons + varerr_cons)/varerr_cons
    #df21n <- n - 1
    #df21d <- (n - 1) * (k - 1)
    #F3L <- F_c/qf(1 - alpha/2, df21n, df21d) #or alpha/2? not dividing by 2 is shrout fleis
    #F3U <- F_c * qf(1 - alpha/2, df21d, df21n)#or alpha/2?
    #L_icc <- (F3L - 1)/(F3L + k - 1)
    #U_icc <- (F3U - 1)/(F3U + k - 1)
    ICC["consistency", "lower"] <- icc_cs$L_icc
    ICC["consistency", "upper"] <- icc_cs$U_icc
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
    return(ICC[method, c("icc", "sem", paste("var", vc$grp, sep = "_"))])
  }
  if(confint == FALSE & sem == FALSE & var == TRUE){
    return(ICC[method, c("icc", paste("var", vc$grp, sep = "_"))])
  }
  if(confint == TRUE & sem == TRUE & var == TRUE){
    return(ICC[method, ])
  }

  if(confint == TRUE & sem == FALSE & var == TRUE){
    return(ICC[method, c("icc",  "lower", "upper", paste("var", vc$grp, sep = "_"))])
  }

}
