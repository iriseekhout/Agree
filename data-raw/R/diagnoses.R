## load data used in Fleis paper library(irr)

library(irr)
data(diagnoses)


save("diagnoses", file = file.path("C:/Users/eekhouti/Github/Agree/data/diagnoses.rda"),
     compress = "xz")
summary(diagnoses)


