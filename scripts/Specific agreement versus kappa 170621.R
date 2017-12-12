#simulation Kappa versus postive/negative agreement

#Riekie BMJ: 2a/2a+b+c voor positive agreement
library(Agree)
library(openxlsx)
library(psych)
Sys.setenv("R_ZIPCMD" ="C:/RBuildTools/3.4/bin/zip.exe")

#NOTES


wb <- createWorkbook()
nsim <-500
n <- 100

#prevalentie varieren van 0-100%
prev1 <- matrix(0,11,2)
prev1[,1] <- seq(0,100, 10)
prev1[,2] <- 100-prev1[,1]
#agreement varieren van 0.1 - 0.9
agset <- seq(0.1,0.9,0.2)

ratervar <- c(2)
y <- 1
nrat <- ratervar[y]
for (p in 1:nrow(prev1)){
  prev <- prev1[p,]
  nlikert <- 2
  #transitie matrix is matrix is kans per categorie (rij totaal is 1; diag is ovarallagreement) - nu voor alle categorieen gelijk
  for (a in seq_along(agset)){
  tmagreement <- agset[a] ##overall agreement
  transmat1 <- matrix((1-tmagreement)/(nlikert-1),nlikert,nlikert)
  diag(transmat1) <- tmagreement

  Ag <- NAg <- PAg <- KAP <- vector()
for (s in 1:nsim){
  nraters <- nrat
  X <- array(rnorm(nraters*n), dim = c(n, nraters))
  M <- matrix(0.6,nraters,nraters)
  diag(M) <- 1
  # adjust correlations for uniforms
  for (i in 1:nraters){
    for (j in max(i, 2):nraters){
      if (i != j){
        M[i, j] <- 2 * sin(pi * M[i, j] / 6)
        M[j, i] <- 2 * sin(pi * M[j, i] / 6)
      }
    }
  }
  # induce correlation, check correlations
  C <- chol(M)
  Y <- X %*% C
  #cor(Y)
  # create uniforms, check correlations
  Y<- pnorm(Y)
  cor(Y)
  p_prev <- (prev/100)/sum((prev/100))
  s_prev <- cumsum(p_prev)
  res <- outer(runif(n), s_prev, ">")
  true <- rowSums(res)+1
  s_trans <- matrix(0,nlikert,nlikert)
  transmat <- transmat1
  for(c in 1:nrow(transmat)){s_trans[c,] <- cumsum(transmat[c,])}
  ratersy <- Y
  for (r in 1:ncol(ratersy)){
    for (k in 1:ncol(s_trans)){
      ratersy[,r][ratersy[,r]<s_trans[true,k]] <- k
    }
  }

  dat <- ratersy
  dat <- data.frame(dat)
  raters <- paste("R", 1:nraters, sep="")
  variable <- c("var")
  colnames(dat) <-paste(raters, variable, sep="_")
  dat[]<-lapply(dat, as.factor)

  Ag <- rbind(Ag,CIagreement(dat))
  PAg <- rbind(PAg,CIagreement(dat, cat1="1"))
  NAg <- rbind(NAg,CIagreement(dat, cat1="2"))
  KAP <- rbind(KAP,  cohen.kappa(sumtable(dat))$confid[1,])

}

AGR0 <- colMeans(Ag)
AGRpos <- colMeans(PAg)
AGRneg <- colMeans(NAg)
KAPPA <- colMeans(KAP)
output <-rbind(AGR0, AGRpos, AGRneg, KAPPA)
colnames(output) <- c("95%low", "Agreement", "95%high")

print(agset[a])
print(prev)
print(output)

addWorksheet(wb,paste0("agreement=",agset[a],"&prev=",paste(prev, collapse = "-")))
writeData(wb, sheet=paste0("agreement=",agset[a],"&prev=",paste(prev, collapse = "-")), output, rowNames = TRUE)

}}



saveWorkbook(wb,paste0("results/Simulation_specificagreement_kappa_2raters", Sys.Date(), ".xlsx"), overwrite = TRUE)
