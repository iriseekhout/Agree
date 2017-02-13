## load data used in Fleis paper library(irr)

library(irr)
data(diagnoses)

diagnoses <- data.frame(diagnoses$rater5, diagnoses$rater1, diagnoses$rater3, diagnoses$rater2, diagnoses$rater6, diagnoses$rater4)
colnames(diagnoses) <- c("rater5","rater1" ,"rater3", "rater2", "rater6", "rater4")


save("diagnoses", file = file.path("C:/Users/eekhouti/Github/Agree/data/diagnoses.rda"),
     compress = "xz")
summary(diagnoses)


