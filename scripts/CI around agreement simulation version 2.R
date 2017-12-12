# CI agreement simulations
load("data/breast.rda")
head(breast)
variable <- "symmetry"
raters <- c("PCH1", "PCH2", "PCH3", "PCH4")
ratersvars <- paste(raters, variable, sep="_")
data1 <- data.frame(breast[ratersvars])
library(Agree)

#real data simulation CI agreement
n <- 100
formCI <- bootCI <- vector()
for (s in 1:25){
  sdat <- data1[sample(1:nrow(data1),n, replace=TRUE),]
  formCI <-rbind(formCI,CIagreement(sdat))
  bootCI <- rbind(bootCI,CIbootagreement(sdat))
}
colMeans(formCI)
colMeans(bootCI)

#bij n = 50 CI +- 0.05 (te breed in CI form)
#bij n = 25 CI +- 0.06 (te breed in CI form)
#bij n = 100 CI +- 0.03 (te breed in CI form)
#bij n = 200 CI +- 0.02 (te breed in CI form)

#n aanpassing door n*srqt(m) (ipv m-1)
#bij n = 200 CI +- 0.01 (te breed in CI form)
#bij n = 50 CI +- 0.04 (te breed in CI form)


#n aanpassing door n*srqt(m-1)*2
#bij n = 200 CI +- 0.01 (te breed in CI form) - bijna on target
#bij n = 50 CI +- 0.02 (te breed in CI form)
#bij n = 100 CI +- 0.015 (te breed in CI form)



#real data simulation CI specific agreement
n <- 200
formCIs1 <- bootCIs1 <- vector()
for (s in 1:25){
  sdat <- data1[sample(1:nrow(data1),n, replace=TRUE),]
  formCIs1 <-rbind(formCIs1,CIagreement(sdat, cat1 = "satisfied"))
  bootCIs1 <- rbind(bootCIs1,CIbootagreement(sdat, cat1 = "satisfied"))
}
colMeans(formCIs1)
colMeans(bootCIs1)

#bij n=50 CI +- 0.6-0.11 te breed bij CI form
#bij n=200 CI +- 0.5 te breed bij CI form

#n aanpassing door n*srqt(m-1)*2
#bij n=200 CI +-0.02 te breed bij CI form
#bij n=100 CI +-0.03 te breed bij CI form
#bij n=50 CI +- 0.04 te breed bij CI form

#n als n bij gewoon CI agreement interval
#bij n=50 CI +- 0.02 te SMAL bij CI form
#bij n=200 CI +- 0.01 te SMAL bij CI form

#n aanpassing /2 weg
#bij n=200 CI +-



#real data simulation CI specific agreement 2
formCIs2 <- bootCIs2 <- vector()
for (s in 1:100){
  sdat <- data1[sample(1:nrow(data1),50, replace=TRUE),]
  formCIs2 <-rbind(formCIs2,CIagreement(sdat, cat1 = "satisfied", cat2 = "dissatisfied"))
  bootCIs2 <- rbind(bootCIs2,CIbootagreement(sdat, cat1 = "satisfied", cat2 = "dissatisfied"))
}
colMeans(formCIs2)
colMeans(bootCIs2)

