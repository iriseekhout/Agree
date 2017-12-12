#Examples paper 2 (polytomous data, multiple raters)
# Title: Specific agreement on polytomous outcomes in the situation of more than two raters Journal of Clinical Epidemiology


## ordinal data example

breast


library(Agree)
library(pander)

load("C:/Users/eekhouti/Github/Agree/data/breast.rda")

variable <- "symmetry"
raters <- c("PCH1", "PCH2", "PCH3", "PCH4")
ratersvars <- paste(raters, variable, sep="_")
data1 <- data.frame(breast[ratersvars])

pander(sumtable(data1,offdiag = FALSE))
pander(sumtable(data1,offdiag = TRUE))

table1 <- sumtable(data1,offdiag = TRUE)
agreement(table1)
CIagreement(agreement(table1),m=4,n=nrow(data1))


spec.agreement2(sumtable(data1), cat1="satisfied")
CIagreement(spec.agreement2(sumtable(data1), cat1="satisfied"), n=nrow(data1), m=4)

spec.agreement2(sumtable(data1), cat1="satisfied", cat2="neutral")
CIagreement(spec.agreement2(sumtable(data1), cat1="satisfied", cat2="neutral"), n=nrow(data1), m=4)

spec.agreement2(sumtable(data1), cat1="satisfied", cat2="neutral")
CIagreement(spec.agreement2(sumtable(data1), cat1="satisfied", cat2="very satisfied"), n=nrow(data1), m=4)



agreement.plusone(table1)
CIagreement(agreement.plusone(table1),m=4,n=nrow(data1))

pander(specific.agreement2(table1))




############


load("C:/Users/eekhouti/Github/Agree/data/diagnoses.rda")
data2 <- data.frame(lapply(diagnoses,as.factor))


sumtable(data2,offdiag = FALSE)
sumtable(data2,offdiag = TRUE)


table2 <- sumtable(data2,ratings = colnames(data2),levels=levels(data2[,1]),offdiag = TRUE)

agreement(table2)
CIagreement(agreement(table2),m=6,n=nrow(data2))

pander(specific.agreement2(table2))


####

#example 1 2 raters from diagnoses data:

dat <- data2[,1:2]
sumtable(dat, offdiag=FALSE)
table2 <- sumtable(dat,ratings = colnames(dat),levels=levels(dat[,1]),offdiag = TRUE)

agreement(table2)
