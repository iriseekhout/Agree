# variance components

## seems not to work
# oneway components from two-way model


library(Agree)
library(lme4)
library(dplyr)
library(tidyr)

data <- breast[,c("PCH1_score", "PCH2_score","PCH3_score",
                 "PCH4_score","PCH5_score")]


#data <- breast[,c("Mam1_score", "Mam2_score","Mam3_score")]


#twoway model
cols <- colnames(data)
k <- length(cols)
n <- nrow(data)
data1 <- data.frame(data) %>%
  mutate(id = 1:nrow(data)) %>% #add id column
  pivot_longer(cols = cols, names_to = "rater", values_to = "score") %>%
  drop_na(score)
twoway <- lmer(score ~ (1|id) + (1|rater), data1, REML = T)

TW_vc <- data.frame(VarCorr(twoway))


varpat_Toneway <- ((k * TW_vc[1,4]) - TW_vc[2,4]) / k
#varpat_Toneway <- ((k * TW_vc[1,4]) + TW_vc[2,4]) / k

varerr_Toneway <- TW_vc[2,4] + TW_vc[3,4]
varpat_Toneway

icc_to <- varpat_Toneway / (varpat_Toneway + varerr_Toneway)
sem_to <- sqrt(varerr_Toneway)



#oneway model:

oneway <- lmer(score ~ (1|id) , data1, REML = T)
OW_vc <- data.frame(VarCorr(oneway))

varpat_Ooneway <- OW_vc[1,4]
varerr_Ooneway <- OW_vc[2,4]
icc_oo <- varpat_Ooneway/(varpat_Ooneway + varerr_Ooneway)
sem_oo <- sqrt(varerr_Ooneway)

icc_to
icc_oo

varpat_Toneway
varpat_Ooneway
varerr_Toneway
varerr_Ooneway



##############################

#check agreement vs consistency from same model
cols <- colnames(data)
k <- length(cols)
n <- nrow(data)
data1 <- data.frame(data) %>%
  mutate(id = 1:nrow(data)) %>% #add id column
  pivot_longer(cols = cols, names_to = "rater", values_to = "score") %>%
  drop_na(score)
twoway1 <- lmer(score ~ (1|id) + (1|rater), data1, REML = T)

TW_vc1 <- data.frame(VarCorr(twoway1))

twoway2 <- lmer(score ~ (1|id) + rater, data1, REML = T)

TW_vc2 <- data.frame(VarCorr(twoway2))

TW_vc1
TW_vc2


