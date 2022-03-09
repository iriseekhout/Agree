#simulation data for Reliability
library(dplyr)
library(tidyr)

#.rda files gekregen van Wieneke (2021/02/23)
#simulation output 1k
load("data-raw/data/output_0ksv.rda")
output0k <- output %>%
  rename(icc = ICC) %>%
  mutate(deviation = 0)
rm(output)


#simulation output 1k
load("data-raw/data/output.rda")
output1k <- output %>%
  rename(icc = ICC) %>%
  mutate(deviation = 1)
rm(output)

#simulation output 2k
load("data-raw/data/output_sv2k.rda")
output2k <- output %>%
  rename(icc = ICC) %>%
  mutate(deviation = 2)
rm(output)

output <- bind_rows(output0k, output1k, output2k) %>%
  rename(cor = icc) %>%
  pivot_longer(cols = c(-set, -n, -cor, -k, -variance, -deviation),
               names_pattern = c("(.*)_(.*)"),
               names_to = c(".value", "method")) %>%
  mutate(method = ifelse(method == "agr", "agreement", method),
         method = ifelse(method == "cons", "consistency", method))


##HIER GEBLEVEN> Data logischer maken, zodat vanuit een bestand alle figuren gemaakt kunnenworden.

#true population data 0k
load("data-raw/data/Populatie_10_10000_0ksv.rda")
colnames(true)[4:9] <- paste("T", colnames(true)[4:9], sep = "")
true0k <- true %>% mutate(deviation = 0)
rm(true)

#true population data 1k
load("data-raw/data/Populatie_10_10000.rda")
colnames(true)[4:9] <- paste("T", colnames(true)[4:9], sep = "")
true1k <- true %>% mutate(deviation = 1)
rm(true)

#true population data
load("data-raw/data/Populatie_10_10000_2ksv.rda")
true2k <- true %>% mutate(deviation = 2)
colnames(true2k)[4:9] <- paste("T", colnames(true2k)[4:9], sep = "")
rm(true)

true <- bind_rows(true0k, true1k, true2k) %>%
  rename(cor = icc) %>%
  pivot_longer(cols = c( -cor, -k, -variance, -deviation),
               names_pattern = c("(.*)_(.*)"),
               names_to = c(".value", "method")) %>%
  mutate(method = ifelse(method == "agr", "agreement", method),
         method = ifelse(method == "cons", "consistency", method))


#edit 9-3-22 > compute CI width from MSE
simoutput <- left_join(output, true) %>%
  mutate(bias_icc = icc - Ticc,
         bias_sem = sem - Tsem,
         mse_icc = bias_icc^2,
         mse_sem = bias_sem^2,
         cov_icc = ifelse(lower < Ticc & upper > Ticc, 1, 0)
         ) %>%
  group_by(n, cor, k, variance, deviation, method) %>%
  summarise(across(everything(), ~mean(.)),
            .groups = "drop") %>%
  mutate(width_icc = (sqrt(mse_icc) * 1.96)*2,
         width_sem = (sqrt(mse_sem) * 1.96)*2
         )


save(simoutput, file =  "data/simoutput.rda")


rm(list = ls())
