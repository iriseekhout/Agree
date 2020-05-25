#' ICC model calculations
#'
#' @param data data frame with k (raters or observers) in columns and n rows.
#' @param cols character vector indicating the column names (observer columns) to be used.
#' @param alpha alpha level used for the confidence intervals.
#'
#' @return list of relevant statisics
icc_model <- function(data, cols = colnames(data), alpha = 0.05){
  k <- length(cols)
  n <- nrow(data)
  data1 <- data.frame(data) %>%
    mutate(id = 1:nrow(data)) %>% #add id column
    pivot_longer(cols = cols, names_to = "observer", values_to = "score")
  REML_model <- lmer(score ~ (1|id) + (1|observer), data1, REML = T)
  vc <- as.data.frame(lme4::VarCorr(REML_model))

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

  #agreement
  varpat_agr <- vc[1,4]
  varobs_agr <- vc[2,4]
  varerr_agr <- vc[3,4]
  icc_a <- varpat_agr/(varpat_agr + varobs_agr + varerr_agr)
  sem_a <- sqrt(varobs_agr + varerr_agr)
  MSB <-  (k * varpat_agr + varerr_agr)
  MSBt <- 1 +(k - 1)*icc_a #test
  MSEt <- 1-icc_a #test
  F_a1 <- (n * varobs_agr + varerr_agr)/varerr_agr

  Fa1 <- MSBt/MSEt #test
  Fl <- qf(alpha/2, df1= (n-1) ,df2=n*(k-1))
  Fu <- qf(1-(alpha/2), df1=(n-1) , df2=n*(k-1))

  l_a <- (Fa1/Fu -1)/(Fa1/Fu+k-1) #test
  u_a <- (Fa1/Fl -1)/(Fa1/Fl+k-1) #test
  #where Fl and Fu are the ˛=2 and the 1  ˛=2th quantiles of the F -distribution    with degrees of freedom
  # N  1 and N.k  1/, respectively.

  vn <- (k - 1) * (n - 1) * ((k * icc_a * F_a1 + n *
                                (1 + (k - 1) * icc_a) - k * icc_a))^2
  vd <- (n - 1) * k^2 * icc_a^2 * F_a1^2 + (n * (1 + (k - 1) * icc_a) - k * icc_a)^2
  v <- vn/vd
  F3U <- qf(1 - alpha/2, n - 1, v)#or alpha/2?not dividing by 2 is shrout fleis
  F3L <- qf(1 - alpha/2, v, n - 1)#or alpha/2?
  L_a <- n * (MSB - F3U * varerr_agr)/(F3U * (k * (n * varobs_agr + varerr_agr) + (k *
                                                                                     n - k - n) * varerr_agr) + n * MSB)#

  U_a <- n * (F3L * MSB - varerr_agr)/(k * (n * varobs_agr + varerr_agr) + (k * n -
                                                                              k - n) * varerr_agr + n * F3L * MSB)
  #consistency
  varpat_cons <- vc[1,4]
  varerr_cons <- vc[3,4]
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
      varpat_oneway = varpat_oneway,
      varerr_oneway = varerr_oneway,
      icc_o = icc_o,
      sem_o = sem_o,
      L_o = L_o,
      U_o = U_o,
      varpat_agr = varpat_agr,
      varobs_agr = varobs_agr,
      varerr_agr = varerr_agr,
      icc_a = icc_a,
      sem_a = sem_a,
      L_a = l_a, #use upper case L for PSYCH version
      U_a = u_a, #use upper case U for PSYCH version
      varpat_cons = varpat_cons,
      varerr_cons = varerr_cons,
      icc_c = icc_c,
      sem_c = sem_c,
      L_c = L_c,
      U_c = U_c
    )
  )
}
