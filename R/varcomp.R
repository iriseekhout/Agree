#' Variance components
#'
#' @param formula a two-sided linear formula object describing both the
#' fixed-effects and random-effects part of the model, with the response on the
#' left of a ~ operator and the terms, separated by + operators, on the right.
#' Random-effects terms are distinguished by vertical bars (|) separating
#' expressions for design matrices from grouping factors. Two vertical bars (||)
#' can be used to specify multiple uncorrelated random effects for the same
#' grouping variable. (Because of the way it is implemented, the ||-syntax
#' works only for design matrices containing numeric (continuous) predictors;
#' to fit models with independent categorical effects, see dummy or the lmer_alt
#' function from the afex package.)
#' @param data a data.frame containing the variables named in `formula`.
#'
#' @return data.frame with the variance components in rows
#' @export
#' @importFrom lme4 lmer VarCorr
#' @importFrom dplyr %>% select
#'
#' @examples
#' form <- score ~ (1|patient)
#' form_cros <- score ~ (1|rater) + (1|patient)
#' #form_nest <- score ~ (1|rater/patient)
#'
#' varcomp(form, data = fake_1)
#' varcomp(form_cros, data = fake_2nest)
#' varcomp(form_cros, data = fake_2cros)
#'
#' form_3nest <- score ~ (1|rater/patient)
#' varcomp(form_3nest, data = fake_3nest %>% drop_na(score))
#'
varcomp <- function(formula, data){

 mod <- lmer(formula, data, REML = T)

 vc <- VarCorr(mod) %>%
   data.frame %>%
   select(.data$grp, .data$vcov)

 vc
}

