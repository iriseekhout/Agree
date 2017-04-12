#paper

library(Agree)
data(breast)

variable <- "symmetry"
raters <- c("PCH1", "PCH2", "PCH3", "PCH4")
ratersvars <- paste(raters, variable, sep="_")
data1 <- data.frame(breast[ratersvars])


cbind(table(data1[,1]), table(data1[,2]), table(data1[,3]), table(data1[,4]))

sumtable(data1, offdiag = FALSE)
sumtable(data1)

agreement(sumtable(data1))


specific.agreement(sumtable(data1), cat1="satisfied")
CIagreement(data1, cat1="satisfied")
CIbootagreement(data1,cat1="satisfied")

specific.agreement(sumtable(data1), cat1="satisfied", cat2="neutral")
CIagreement(sumtable(data1), cat1="satisfied", cat2="neutral", n=nrow(data1), m=4)
CIbootagreement(data1, cat1="satisfied", cat2="neutral")


data(diagnoses)
data2 <- diagnoses
sumtable(data2, offdiag = FALSE)


agreement(sumtable(data2))

CIagreement(specific.agreement(sumtable(data2), cat1="1. Depression"), n=nrow(data2), m=6)
CIagreement(specific.agreement(sumtable(data2), cat1="2. Personality Disorder"), n=nrow(data2), m=6)
CIagreement(specific.agreement(sumtable(data2), cat1="3. Schizophrenia"), n=nrow(data2), m=6)
CIagreement(specific.agreement(sumtable(data2), cat1="4. Neurosis"), n=nrow(data2), m=6)
CIagreement(specific.agreement(sumtable(data2), cat1="5. Other"), n=nrow(data2), m=6)


conditional.agreement(sumtable(data2))


### extra checks
##


agreement(sumtable(data1))

SA1 <-agreement(sumtable(data1, ratings = c("PCH1_symmetry", "PCH2_symmetry")))
SA2 <-agreement(sumtable(data1, ratings = c("PCH1_symmetry", "PCH3_symmetry")))
SA3 <-agreement(sumtable(data1, ratings = c("PCH1_symmetry", "PCH4_symmetry")))
SA4 <-agreement(sumtable(data1, ratings = c("PCH2_symmetry", "PCH3_symmetry")))
SA5 <-agreement(sumtable(data1, ratings = c("PCH2_symmetry", "PCH4_symmetry")))
SA6 <-agreement(sumtable(data1, ratings = c("PCH3_symmetry", "PCH4_symmetry")))

mean(c(SA1,SA2, SA3,SA4,SA5,SA6))

##Komt light manier ook overeen bij SA?

specific.agreement(sumtable(data1),  cat1="satisfied", cat2="neutral")

SA1 <-specific.agreement(sumtable(data1, ratings = c("PCH1_symmetry", "PCH2_symmetry")), cat1="satisfied", cat2="neutral")
SA2 <-specific.agreement(sumtable(data1, ratings = c("PCH1_symmetry", "PCH3_symmetry")), cat1="satisfied", cat2="neutral")
SA3 <-specific.agreement(sumtable(data1, ratings = c("PCH1_symmetry", "PCH4_symmetry")), cat1="satisfied", cat2="neutral")
SA4 <-specific.agreement(sumtable(data1, ratings = c("PCH2_symmetry", "PCH3_symmetry")), cat1="satisfied", cat2="neutral")
SA5 <-specific.agreement(sumtable(data1, ratings = c("PCH2_symmetry", "PCH4_symmetry")), cat1="satisfied", cat2="neutral")
SA6 <-specific.agreement(sumtable(data1, ratings = c("PCH3_symmetry", "PCH4_symmetry")), cat1="satisfied", cat2="neutral")

mean(c(SA1,SA2, SA3,SA4,SA5,SA6))

# hier even testen


specific.agreement(sumtable(data1), cat1="satisfied")

SA1 <-specific.agreement(sumtable(data1, ratings = c("PCH2_symmetry", "PCH1_symmetry")), cat1="satisfied")
SA2 <-specific.agreement(sumtable(data1, ratings = c("PCH1_symmetry", "PCH3_symmetry")), cat1="satisfied")
SA3 <-specific.agreement(sumtable(data1, ratings = c("PCH4_symmetry", "PCH1_symmetry")), cat1="satisfied")
SA4 <-specific.agreement(sumtable(data1, ratings = c("PCH2_symmetry", "PCH3_symmetry")), cat1="satisfied")
SA5 <-specific.agreement(sumtable(data1, ratings = c("PCH2_symmetry", "PCH4_symmetry")), cat1="satisfied")
SA6 <-specific.agreement(sumtable(data1, ratings = c("PCH3_symmetry", "PCH4_symmetry")), cat1="satisfied")

mean(c(SA1,SA2, SA3,SA4,SA5,SA6))

SE <- (sqrt(1/n*(p*(1-p))))-1/(2*n))
# weighted means met (1/SE^2)
#

specific.agreement(sumtable(data1), cat1="dissatisfied")

SA1 <-specific.agreement(sumtable(data1, ratings = c("PCH1_symmetry", "PCH2_symmetry")), cat1="dissatisfied")
SA2 <-specific.agreement(sumtable(data1, ratings = c("PCH1_symmetry", "PCH3_symmetry")), cat1="dissatisfied")
SA3 <-specific.agreement(sumtable(data1, ratings = c("PCH1_symmetry", "PCH4_symmetry")), cat1="dissatisfied")
SA4 <-specific.agreement(sumtable(data1, ratings = c("PCH2_symmetry", "PCH3_symmetry")), cat1="dissatisfied")
SA5 <-specific.agreement(sumtable(data1, ratings = c("PCH2_symmetry", "PCH4_symmetry")), cat1="dissatisfied")
SA6 <-specific.agreement(sumtable(data1, ratings = c("PCH3_symmetry", "PCH4_symmetry")), cat1="dissatisfied")

mean(c(SA1,SA2, SA3,SA4,SA5,SA6))


### ook voor diagnoses

agreement(sumtable(data2))

SA1 <-agreement(sumtable(data2, ratings = c("rater1", "rater2")))
SA2 <-agreement(sumtable(data2, ratings = c("rater1", "rater3")))
SA3 <-agreement(sumtable(data2, ratings = c("rater1", "rater4")))
SA4 <-agreement(sumtable(data2, ratings = c("rater1", "rater5")))
SA5 <-agreement(sumtable(data2, ratings = c("rater2", "rater3")))
SA6 <-agreement(sumtable(data2, ratings = c("rater2", "rater4")))
SA7 <-agreement(sumtable(data2, ratings = c("rater2", "rater5")))
SA8 <-agreement(sumtable(data2, ratings = c("rater3", "rater4")))
SA9 <-agreement(sumtable(data2, ratings = c("rater3", "rater5")))
SA10 <-agreement(sumtable(data2, ratings = c("rater4", "rater5")))
SA11 <-agreement(sumtable(data2, ratings = c("rater1", "rater6")))
SA12 <-agreement(sumtable(data2, ratings = c("rater2", "rater6")))
SA13 <-agreement(sumtable(data2, ratings = c("rater3", "rater6")))
SA14 <-agreement(sumtable(data2, ratings = c("rater4", "rater6")))
SA15 <-agreement(sumtable(data2, ratings = c("rater5", "rater6")))

mean(c(SA1,SA2, SA3,SA4,SA5,SA6,SA7,SA8,SA9,SA10,SA11,SA12,SA13,SA14,SA15))


###

specific.agreement(sumtable(data2), cat1="1. Depression")

SA1 <-specific.agreement(sumtable(data2, ratings = c("rater1", "rater2")), cat1="1. Depression")
SA2 <-specific.agreement(sumtable(data2, ratings = c("rater1", "rater3")), cat1="1. Depression")
SA3 <-specific.agreement(sumtable(data2, ratings = c("rater1", "rater4")), cat1="1. Depression")
SA4 <-specific.agreement(sumtable(data2, ratings = c("rater1", "rater5")), cat1="1. Depression")
SA5 <-specific.agreement(sumtable(data2, ratings = c("rater2", "rater3")), cat1="1. Depression")
SA6 <-specific.agreement(sumtable(data2, ratings = c("rater2", "rater4")), cat1="1. Depression")
SA7 <-specific.agreement(sumtable(data2, ratings = c("rater2", "rater5")), cat1="1. Depression")
SA8 <-specific.agreement(sumtable(data2, ratings = c("rater3", "rater4")), cat1="1. Depression")
SA9 <-specific.agreement(sumtable(data2, ratings = c("rater3", "rater5")), cat1="1. Depression")
SA10 <-specific.agreement(sumtable(data2, ratings = c("rater4", "rater5")), cat1="1. Depression")
SA11 <-specific.agreement(sumtable(data2, ratings = c("rater1", "rater6")), cat1="1. Depression")
SA12 <-specific.agreement(sumtable(data2, ratings = c("rater2", "rater6")), cat1="1. Depression")
SA13 <-specific.agreement(sumtable(data2, ratings = c("rater3", "rater6")), cat1="1. Depression")
SA14 <-specific.agreement(sumtable(data2, ratings = c("rater4", "rater6")), cat1="1. Depression")
SA15 <-specific.agreement(sumtable(data2, ratings = c("rater5", "rater6")), cat1="1. Depression")
mean(c(SA1,SA2, SA3,SA4,SA5,SA6,SA7,SA8,SA9,SA10,SA11,SA12,SA13,SA14,SA15))



## Bootstrap CI check voor wat voorbeelden.
#
## data1
CIagreement(sumtable(data1),n=nrow(data1), m=4)
CIbootagreement(data1)

CIagreement(data1,cat1 = "satisfied")
CIbootagreement(data1,cat1 = "satisfied")


CIagreement(data1,cat1 = "satisfied", cat2="very satisfied")
CIbootagreement(data1,cat1 = "satisfied", cat2="very satisfied")

