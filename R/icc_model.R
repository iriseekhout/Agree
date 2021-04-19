#' ICC model
#'
#' Calculate model for the ICC estimations. The basic model contains a random
#' intercept for the id-level and a random intercept for the observer level.
#' For each ICC type, different variance components are used.
#'
#' @param data data.frame with a column for each observer/rater and a row per
#' rated subject.
#' @param cols character vector with the column names to be used as observers.
#' Default is `cols = colnames(data)`.
#'
#' @importFrom lme4 lmer
#' @importFrom tidyr pivot_longer
#' @importFrom dplyr %>% mutate
#' @return `lmer`model object
#' @export
#'
icc_model <- function(data, cols = colnames(data)){
  k <- length(cols)
  n <- nrow(data)
  data1 <- data.frame(data) %>%
    mutate(id = 1:nrow(data)) %>% #add id column
    pivot_longer(cols = cols, names_to = "observer", values_to = "score")
  REML_model <- lmer(score ~ (1|id) + (1|observer), data1, REML = T)
  REML_model
}

