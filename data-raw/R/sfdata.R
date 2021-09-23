#shrout fleis

library(haven)

sfdata_long <- read_sav("data-raw/data/ANOVA DATA SET_SHROUT&FLEISS_long version.sav")

sfdata_wide <- read_sav("data-raw/data/ANOVA DATA SET_SHROUT&FLEISS_short version.sav")[,-1] %>%
  mutate_all(as.vector)



save("sfdata_long", file = file.path("data/sfdata_long.rda"),
     compress = "xz")

save("sfdata_wide", file = file.path("data/sfdata_wide.rda"),
     compress = "xz")
