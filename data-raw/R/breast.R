#import breast data

dat1 <- read.delim("C:/Users/eekhouti/Github/Agree/data-raw/data/Data validatiestudie versie 3.txt",dec=",")
dat1[dat1==9999]<- NA  ## 9999 is missing hercoderen


## variabele namen in dat moeten gespecificeerd zijn als "[rater]"_"[variable]"
variables_available <- c("volume", "shape", "symmetry", "scars", "nipple")
raters_available <- c("Patient", "PCH1", "PCH2", "PCH3", "PCH4", "PCH5", "Mam1", "Mam2", "Mam3")

breast <- dat1
save("breast", file = file.path("C:/Users/eekhouti/Github/Agree/data/breast.rda"),
     compress = "xz")
rm(dat1)
summary(breast)


