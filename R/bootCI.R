#' bootCI
#'
#' @param data data.frame for which the bootstraps need to be performed to obtain
#' confidence intervals.
#' @param b number of bootstrap replications
#' @param alpha level of Confidence interval, default \code{alpha = 0.05}
#' @param fun function to estimate parameter for which the CI is determined.
#' @param \dots additional parameters for function
#' @importFrom boot boot
#' @return named vector with confidence interval levels
#' @export
#' @examples
#' \dontrun{
#' agreement(diagnoses)
#' bootCI(data = diagnoses, fun = agreement)
#' agreement(diagnoses, specific = "4. Neurosis")
#' bootCI(data = diagnoses, fun = agreement, specific = "4. Neurosis")
#' ## bootsctrap CI for SEM agreement
#' icc(data = breast[,c("PCH1_score", "PCH2_score", "PCH3_score")],
#' method = "agreement", confint = FALSE)["sem"]
#' sem_a <- function(x){icc(data = x, method = "agreement", confint = FALSE)["sem"]}
#' sem_a <- function(x){unlist(icc_agreement(x)["sem"])}
#' bootCI(data = breast[,c("PCH1_score", "PCH2_score", "PCH3_score")],
#' fun = sem_a)
#' }
bootCI <- function(data,
                   b=1000,
                   alpha=0.05,
                   fun,
                   ...){
  stopifnot(alpha>=0|alpha<=1)
    boot.fun <- function(data,x) {
      fun(data[x,], ...)
      }
  res1a <- boot::boot(data,boot.fun,b)
  BCI <-  quantile(res1a$t,c((alpha)/2,(1-alpha)+((alpha)/2)), na.rm=TRUE)
  names(BCI) <- c("lower", "upper")
  BCI
}
