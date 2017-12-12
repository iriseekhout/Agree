#Examples paper 1 (dichotomous data, multiple raters)
# Title: Specific agreement on dichotomous outcomes in the situation of more than two raters Journal of Clinical Epidemiology

data(breast)

variables_available <- c("volume", "shape", "symmetry", "scars", "nipple")
raters_available <- c("Patient", "PCH1", "PCH2", "PCH3", "PCH4", "PCH5", "Mam1", "Mam2", "Mam3")

#!# dichotomiseren ratersdata 1 means satisfied, 2 means not satisfied
datdich <- breast
for (v in 1:length(variables_available)){
  ratersvars1 <- paste(raters_available, variables_available[v], sep="_")
  for (r in 1:length(ratersvars1)){
    datdich[ratersvars1[r]] <- ifelse(datdich[ratersvars1[r]]>3,1,2)
  }
}


variable <- variables_available[3]
raters <- c(raters_available[2:5])
ratersvars <- paste(raters, variable, sep="_")

data1 <- data.frame(apply(datdich[ratersvars],2,as.factor))

sumtab <- sumtable(data1,ratings = ratersvars,levels=c(1,2))
sumtab

agreement(sumtab)

specific.agreement1(sumtab,"positive")
specific.agreement1(sumtab,"negative")

CIagreement(agreement(sumtab),n=nrow(data1),m=4)


#######################
#appendix A.6 example
#Calculation of observed agreement and specific agreements for the situation of 86% “satisfied’ scores

table1<-matrix(c(220,38,38,4),nrow=2,byrow = TRUE)

agreement(table1)

specific.agreement1(table1,"positive")
specific.agreement1(table1,"negative")

