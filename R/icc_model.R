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
    mutate(level1 = 1:nrow(data)) %>% #add id column
    pivot_longer(cols = cols, names_to = "level2", values_to = "score")
  REML_model <- lmer(score ~ (1|level1) + (1|level2), data1, REML = T)
  REML_model
}


#' ICC model
#'
#' Calculate model for the ICC estimations. The basic model contains a random
#' intercept for the id-level and a random intercept for the rater level, more
#' levels can be indicated for a 3-way model.This function works from a long data
#' format.
#' For each ICC type, different variance components are used.
#'
#' @param data data.frame in a long format with multiple rows per subject (id).
#' The data should contain a column for each of the `levels` and a column named
#' `score` for the scores.
#' @param levels character vector with the column names of the levels that should
#' be used in the data. Default is `cols = c("id", "rater")`.
#' @param values character with name of variable with the score values,
#' default = "score".
#'
#' @importFrom lme4 lmer
#' @importFrom tidyr pivot_longer
#' @importFrom dplyr %>% mutate
#' @return `lmer`model object
#' @export
#' @examples
#' breast_scores <- Agree::breast %>%
#'   dplyr::select(Patient_score, PCH1_score, PCH2_score, PCH3_score,
#'   Mam1_score, Mam2_score, Mam3_score)
#'data1 <- breast_scores %>%
#'mutate(id = 1:nrow(breast_scores)) %>% #add id column
#'pivot_longer(cols = -id, names_to = "rater", values_to = "score")
#'icc_model2(data = data1)
icc_model2 <- function(data, levels = c("id", "rater"), values = "score"){

  #n <- n_distinct(data[,levels[1]])
  #k <- n_distinct(data[,levels[2]])
df <- data[c(levels, values)]
 if(length(levels) == 1){
   colnames(df) <- c("level1", "score")
   form <- as.formula(paste0("score ~ (1|level1)"))
    }

if(length(levels) == 2){
  colnames(df) <- c("level1","level2", "score")
  form <- as.formula(paste0("score ~ (1|level1) + (1|level2)"))
}

  if(length(levels) == 3){
    colnames(df) <- c("level1","level2","level3", "score")
    form <- as.formula(paste0("score ~ (1|level1) + (1|level2) + (1|level3)"))
  }

  REML_model <- lmer(form, df, REML = T)
  REML_model


}
