
library(boot)
library(dplyr)
library(tidyr)
library(Agree)

breast_scores <-
       Agree::breast %>%
       dplyr::select(Patient_score, PCH1_score, PCH2_score, PCH3_score, PCH4_score,
                      PCH5_score, Mam1_score, Mam2_score, Mam3_score)
 breast_long <- breast_scores %>%
       mutate(id = 1:nrow(breast_scores)) %>% #add id column
       pivot_longer(cols = -id, names_to = "rater", values_to = "score")

   vc <- varcomp(score~ (1|id) + (1|rater), data = breast_long)
   vc["id", "vcov"]/sum(vc[,"vcov"])


  # custom function for icc agreement:
   icc_agree_custom <- function(data, idx){

     # bootstrap sample van brede data file (regel per subject id)
     df <- data[idx,]

     #brede data lang maken - kan voor meer levels ook in de pivot_longer functie
     df_long <- df %>%
       mutate(id = 1:nrow(df)) %>% #add id column
       pivot_longer(cols = -id, names_to = "rater", values_to = "score")

     #varcomp functie met gewenste model
     vc <- varcomp(score~ (1|id) + (1|rater), data = df_long)
     #icc berekening
     vc["id", "vcov"]/sum(vc[,"vcov"])
   }


   set.seed(546)


   bootstrap <- boot(breast_scores, icc_agree_custom, R = 1000)

   bootstrap

   boot.ci(boot.out = bootstrap,
           type= "bca")

# ter vergelijk
   icc_agreement(breast_scores)
   icc_agreement(breast_scores, CI_estimator = "approx")
