#check and compare two ways of icc calculations >> compare to ICC from psych package too
data <- breast[,c("PCH1_score", "PCH2_score","PCH3_score",
                  "PCH4_score","PCH5_score")]



data <- breast[,c("Mam1_score", "Mam2_score","Mam3_score")]


library(MASS)

sig <- matrix(0.7, nrow=5, ncol = 5)
diag(sig) <- 1
sig <- sig *5
data <- mvrnorm(n = 100, mu = c(0,1,2,0,0), Sigma = sig)
colnames(data) <- paste0("X", 1:5)

data <- sfdata_wide #Shrout & Fleiss voorbeeld data

#all icc's computed from one model:
icc(data, var = T, onemodel = T)
icc(data, var = T, onemodel = F)
#all icc's computed from a separate model - using the official model per icc:
#icc2(data, var = T)
#programation by psych library via lmer
psych::ICC(data, lmer = TRUE)
#prgram by psych via anova
psych::ICC(na.omit(data), lmer = FALSE)


icc_agreement(data, CI_estimator = "approx" )
icc_agreement(data, CI_estimator = "exact" )
