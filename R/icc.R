#' Intra class correlation for rater reliability
#'
#' @param data data.frame with repeated measures or observations in the columns
#'   and rated subjects in the rows
#' @param method type of ICC that is returned, options are:
#' `c("oneway", "agreement", "consistency")`, the default returns all.
#' @param cols column names used for the repeated measures in the wide format,
#' default uses `cols = colnames(data)`
#' @param sem logical vector if standard error of measurement is returned.
#' @param confint logical vector if confidence interval for ICC is returned.
#' @param alpha the confidence level required, default `alpha = 0.05`.
#' @param var logical indicator if variance estimates are returned.
#' @param onemodel logical indicator if all ICC's should be computed from one
#' model to increase computation time, defalt `onemodel = FALSE`.
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
                cols = colnames(data),
                sem = TRUE,
                confint = TRUE,
                alpha = 0.05,
                var = FALSE,
                onemodel = FALSE){



  ICC <- matrix(NA, nrow = length(method), ncol = (7))
  rownames(ICC) <- method
  colnames(ICC) <- c("icc", "lower", "upper", "sem", "var_level1", "var_level2", "var_Residual")



  if("oneway" %in% method){
   icc_ow <- icc_oneway(data = data, cols = cols, alpha = alpha, twoway = onemodel)

  ICC["oneway", paste("var_level1")] <- icc_ow$varj_oneway
  ICC["oneway", paste("var_Residual")] <- icc_ow$varerr_oneway

  ICC["oneway", "icc"] <- icc_ow$icc
  ICC["oneway", "sem"] <- icc_ow$sem

  ICC["oneway", "lower"] <- icc_ow$L_icc
  ICC["oneway", "upper"] <- icc_ow$U_icc
  }

  if("agreement" %in% method){

   icc_am <- icc_agreement(data = data, cols = cols, alpha = alpha)


  ICC["agreement", paste("var_level1")] <- icc_am$varj_agr
  ICC["agreement", paste("var_Residual")] <- icc_am$varerr_agr
  ICC["agreement", paste("var_level2")] <- icc_am$varr_agr

  ICC["agreement", "icc"] <- icc_am$icc
  ICC["agreement", "sem"] <- icc_am$sem

  ICC["agreement", "lower"] <- icc_am$L_icc
  ICC["agreement", "upper"] <- icc_am$U_icc

  }

  if("consistency" %in% method){
  icc_cs <- icc_consistency(data = data, cols = cols, alpha = alpha, twoway = onemodel)


  ICC["consistency", paste("var_level1")] <- icc_cs$varj_cons
  ICC["consistency", paste("var_Residual")] <- icc_cs$varerr_cons

  ICC["consistency", "icc"] <- icc_cs$icc
  ICC["consistency", "sem"] <- icc_cs$sem

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
    return(ICC[method, c("icc", "sem","var_level1", "var_level2", "var_Residual")])
  }
  if(confint == FALSE & sem == FALSE & var == TRUE){
    return(ICC[method, c("icc", "var_level1", "var_level2", "var_Residual")])
  }
  if(confint == TRUE & sem == TRUE & var == TRUE){
    return(ICC[method, ])
  }

  if(confint == TRUE & sem == FALSE & var == TRUE){
    return(ICC[method, c("icc",  "lower", "upper","var_level1", "var_level2", "var_Residual")])
  }

}


