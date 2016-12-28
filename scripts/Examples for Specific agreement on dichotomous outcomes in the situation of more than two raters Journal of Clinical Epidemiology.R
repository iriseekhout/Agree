#Examples paper 1 (dichotomous data, multiple raters)
# Title: Specific agreement on dichotomous outcomes in the situation of more than two raters Journal of Clinical Epidemiology

data(breast)

variables_available <- c("volume", "shape", "symmetry", "scars", "nipple")
raters_available <- c("Patient", "PCH1", "PCH2", "PCH3", "PCH4", "PCH5", "Mam1", "Mam2", "Mam3")

#!# dichotomiseren ratersdata
datdich <- breast
for (v in 1:length(variables_available)){
  ratersvars1 <- paste(raters_available, variables_available[v], sep="_")
  for (r in 1:length(ratersvars1)){
    datdich[ratersvars1[r]] <- ifelse(datdich[ratersvars1[r]]>3,2,1)
  }
}


#!# hier aangeven welke variabele je wilt bekijken van de bovenstaande variables available.
variable <- variables_available[3]
#!# hier selecteren welke raters je wilt vergelijken van de bovenstaande raters available.
raters <- c(raters_available[2:5])
#!# aantal categorien Likert schaal
ratersvars <- paste(raters, variable, sep="_")

data1 <- data.frame(apply(datdich[ratersvars],2,as.factor))

sumtab <- sumtable(data1,ratings = ratersvars,levels=c(1,2))
sumtab

agreement(sumtab)

specific.agreement1(sumtab,"positive")
specific.agreement1(sumtab,"negative")

CIagreement(agreement(sumtab),n=nrow(data1),m=4)

