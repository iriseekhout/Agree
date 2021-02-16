#global
library(dplyr)
library(tidyr)
library(ggplot2)
library(shinythemes)
library(shinydashboard)


#load("inst/ICCpower/output_mse_ICC.rda")
load("output_mse_ICC.rda")
output_mse_1r <- output_mse %>% mutate(sk = 1)

#load("inst/ICCpower/output_mse_ICC_sv2k.rda")
load("output_mse_ICC_sv2k.rda")
output_mse_2r <- output_mse %>% mutate(sk = 2)

output_mse <- bind_rows(output_mse_1r, output_mse_2r)
colnames(output_mse) <- gsub("_sqe", "", colnames(output_mse))


output_mse <- output_mse %>%
  rename(cor = icc) %>%
  pivot_longer(cols = icc_oneway:sem_cons,
               names_to = c(".value", "type"),
               names_pattern = "(.*)_(.*)"
               )

# ## input parameters
# correlation <- 0.7
# variance <- 1
# icc <- "oneway"
# systdif <- 1
#
#
# ## show results for MSE current contitions
# disdat <- output_mse %>%
#   mutate(n = factor(n),
#          k = factor(k)) %>%
#   filter(cor == !!correlation & variance == !!variance, type == !!icc, sk == !!systdif)
#
# ggplot(disdat, aes(x = k, y = icc, group = n, color = n))+
#   geom_line() +
#   ylab("MSE for icc") +
#   xlab("Raters (k)")
#
#
# ggplot(disdat, aes(x = k, y = sem, group = n, color = n))+
#   geom_line() +
#   ylab("MSE for sem") +
#   xlab("Raters (k)")
#

## calculate MSE ratio's with settings.

mser <- function(n = 10, k = 3, increase = "k",
                 correlation = 0.7, variance = 1, icc = "oneway", systdif = 1){
if(increase == "k"){
  disdat <- output_mse %>%
    filter(cor == !!correlation & variance == !!variance, type == !!icc, sk == !!systdif) %>%
    filter(n == !!n)
  iccref <- disdat[disdat$k == k,"icc"]

  fratios <- unlist(iccref)/disdat$icc
  ks <- disdat$k
  res <- cbind(k = ks, n = n * fratios)
  }

  if(increase == "n"){
    disdat <- output_mse %>%
      filter(cor == !!correlation & variance == !!variance, type == !!icc, sk == !!systdif) %>%
      filter(k == !!k)
    iccref <- disdat[disdat$n == n,"icc"]

    fratios <- unlist(iccref)/disdat$icc
    ns <- disdat$n
    res <- cbind(n = ns, k = k * fratios)
  }

  res
}

# out <- mser(n = 25, k = 3, increase = "k")
#
# ggplot(data.frame(out), aes(x = k, y = n))+
#   geom_point()

# dat inbouwen in interactieve app.
# gebruikers kunnen de data parameters invullen (die nu al ingeprogrammeerd staan)
# aangeven wat kun uitgangspunt is voor k en n en dan aangeven wat ze mogelijk willen laten toenemen om te zien wat de winst daar dan van is.
