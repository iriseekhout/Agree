#make appendices for paper 2
library(openxlsx)
Sys.setenv("R_ZIPCMD" ="C:/RBuildTools/3.4/bin/zip.exe")
library(Agree)

#Appendix 1 dataset1
load("data/breast.rda")

variable <- "symmetry"
raters <- c("PCH1", "PCH2", "PCH3", "PCH4")
ratersvars <- paste(raters, variable, sep="_")
data1 <- data.frame(breast[ratersvars])

write.xlsx(data1, "results/Paper2 Appendix 1.xlsx")

#Appendix 2 dataset2
load("data/diagnoses.rda")
data2 <- data.frame(lapply(diagnoses,as.factor))
write.xlsx(diagnoses, "results/Paper2 Appendix 2.xlsx")

#Appendix 3 5x5 tables for dataset1
data1[] <- lapply(data1, as.factor)
t1.2 <- table(data1[,1:2])
t1.3 <- table(data1[,c(1,3)])
t1.4 <- table(data1[,c(1,4)])
t2.3 <- table(data1[,2:3])
t2.4 <- table(data1[,c(2,4)])
t3.4 <- table(data1[,3:4])

wb1 <- createWorkbook()
addWorksheet(wb1, "surgeon1vs2")
addWorksheet(wb1, "surgeon1vs3")
addWorksheet(wb1, "surgeon1vs4")
addWorksheet(wb1, "surgeon2vs3")
addWorksheet(wb1, "surgeon2vs4")
addWorksheet(wb1, "surgeon3vs4")
writeData(wb1,"surgeon1vs2", t1.2)
writeData(wb1,"surgeon1vs3", t1.3)
writeData(wb1,"surgeon1vs4", t1.4)
writeData(wb1,"surgeon2vs3", t2.3)
writeData(wb1,"surgeon2vs4", t2.4)
writeData(wb1,"surgeon3vs4", t3.4)
saveWorkbook(wb1, "results/Paper3 Appendix 3.xlsx", overwrite = TRUE)


