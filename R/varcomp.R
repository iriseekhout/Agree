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
#' @importFrom dplyr %>% select .data
#'
#' @examples
#' pch <- breast[,c("PCH1_score", "PCH2_score","PCH3_score",
#' "PCH4_score","PCH5_score")] %>%
#' mutate(id = 1:nrow(breast))
#' pch_l <- tidyr::pivot_longer(pch, cols= PCH1_score:PCH5_score,
#' names_to = "rater", values_to = "score")
#' varcomp(formula = score ~ (1|id) + (1|rater),  data = pch_l)
#'
varcomp <- function(formula, data){

 mod <- lmer(formula, data, REML = T)

 vc <- VarCorr(mod) %>%
   data.frame %>%
   select(.data$grp, .data$vcov)
 rownames(vc) <- vc$grp

 vc
}

