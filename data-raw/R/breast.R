#import breast data

dat1 <- read.delim("C:/Users/eekhouti/Github/Agree/data-raw/data/Data validatiestudie versie 3.txt",dec=",")
dat1[dat1==9999]<- NA  ## 9999 is missing hercoderen
dat1[dat1==8888]<- NA  ## 9999 is missing hercoderen
dat1[dat1==6]<- NA  ## 6 is missing hercoderen

## variabele namen in dat moeten gespecificeerd zijn als "[rater]"_"[variable]"
variables_available <- c("volume", "shape", "symmetry", "scars", "nipple")
raters_available <- c("Patient", "PCH1", "PCH2", "PCH3", "PCH4", "PCH5", "Mam1", "Mam2", "Mam3")

#make factor levels & dichotomous version of ratings
levelnames <- function(x){
  x <- factor(x, levels=c(1,2,3,4,5),labels=c("very dissatisfied", "dissatisfied", "neutral", "satisfied", "very satisfied"))}
levelnames2 <- function(x){
  x <- factor(x, levels = c(1,2), labels=c("satisfied", "dissatisfied"))}

for(k in variables_available) {
  dat1[,paste(paste(raters_available,k,sep="_"),"D", sep="_")] <- ifelse(dat1[,paste(raters_available,k,sep="_")]>3,1,2)
  dat1[,paste(paste(raters_available,k,sep="_"),"D", sep="_")] <- data.frame(lapply(dat1[,paste(paste(raters_available,k,sep="_"),"D", sep="_")],levelnames2))
  dat1[,paste(raters_available,k,sep="_")] <- data.frame(lapply(dat1[,paste(raters_available,k,sep="_")],levelnames))
}

breast <- dat1

save("breast", file = file.path("C:/Users/eekhouti/Github/Agree/data/breast.rda"),
     compress = "xz")
rm(dat1)
head(breast)




