#' ICC model
#'
#' Calculate model for the ICC estimations. The basic model contains a random intercept for the id-level and a random intercept for the observer level. For the ICC calculations, different variance and error components are used.
#'
#' @param data data.frame with observerd in the columns and subjects in the rows
#' @param cols character vector with the column names to be used as observers.
#'
#' @importFrom lme4 lmer
#' @importFrom tidyr pivot_longer
#' @importFrom dplyr %>%
#' @return
#' @export
#'
icc_model <- function(data, cols = colnames(data)){
  k <- length(cols)
  n <- nrow(data)
  data1 <- data.frame(data) %>%
    mutate(id = 1:nrow(data)) %>% #add id column
    pivot_longer(cols = cols, names_to = "observer", values_to = "score")
  REML_model <- lme4::lmer(score ~ (1|id) + (1|observer), data1, REML = T)
  REML_model
}

