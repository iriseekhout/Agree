# CI via bootstrap versus CI van p value
#CI van p-value = 1.96*(sqrt(1/n*p(1-p)))
# specific agreement nog inbouwen met bijbehorende CI's - opzoeken in papers

#Riekie BMJ: 2a/2a+b+c voor positive agreement
library(Agree)

#NOTES
#hier nu twee opties van aantal categorieen, een keer 2 categorieen (vgl met specific agreement paper1) en een keer met 4 categorieen. Zelfde algorithmes en functies verder gebruiken.


nsim <-50
n <- 100
prev1 <- c(30,70)
prev2 <- c(10,90)
prev3 <- c(20,80)
prev4 <- c(50,50)
preva <-rbind(prev1,prev2,prev3,prev4)
nlikertvar <- c(2,4)

ratervar <- c(8)
#mate van overeenstemming tussen raters
transmat1 <- matrix(0.20, nlikert,nlikert, byrow=TRUE)
diag(transmat1) <- 1-((nlikert-1)*0.20)
# BCIs <- PCIs <- PCI2s <- PCI3s <- PCI4s <-FCIs <- FCI2s <- FCI3s <- FCI4s <-PCI5s <- FCI5s <-  vector()
# BCIposs <- PCIa1s <- PCIa12s <- PCIa13s <- PCIa14s <-FCIa1s <- FCIa12s <- FCIa13s <- FCIa14s <-PCIa15s <- FCIa15s <- vector()
# BCInegs <- PCIa2s <- PCIa22s <- PCIa23s <- PCIa24s <-FCIa2s <- FCIa22s <- FCIa23s <- FCIa24s <-PCIa25s <- FCIa25s <- vector()

  y <- 1
  nrat <- ratervar[y]
  prev <- prev1
  for (l in 1:2){ ##in plaats van prevalentie variate, variatie voor aantal categorieen.
  nlikert <- nlikertvar[l]
  CIagreement0 <- CIagreement1 <- CIagreement2 <-CIagreement0F <- CIagreement1F <- CIagreement2F <-  BCIagreement0 <- BCIagreement1 <- BCIagreement2 <- vector()
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
  ratersvars <-  paste(raters,variable, sep = "_") ### even omgedraaid voor dit voorbeeld!!
  crosval <- t(combn(ratersvars, 2))
  data <- dat

  crostab <- list()
  for (z in 1:nrow(crosval)) {
    v1 <- dat[crosval[z,1]][,1]
    v1.1 <- factor(v1, levels = 1:nlikert)
    v2 <- dat[crosval[z,2]][,1]
    v2.1 <- factor(v2, levels = 1:nlikert)
    table  <- ftable(v1.1, v2.1)
    crostab[[z]] <- table
    if (z == 1) {
      sumtab <- crostab [[z]]
    }
    if (z > 1) {
      sumtab <- sumtab + crostab[[z]]
    }
  }

  data[]<-lapply(data, as.factor)
  CIagreement0i <- CIagreement(data) #overall agreement
  CIagreement1i <- CIagreement(data,cat1="1") #positive agreement - 1 versus rest
  CIagreement2i <- CIagreement(data,cat1="2", cat2="1") #negative agreement/agreement 2 vs 1

  CIagreement0iF <- CIagreement(data,correction ="Fleis" ) #overall agreement
  CIagreement1iF <- CIagreement(data,cat1="1",correction ="Fleis" ) #positive agreement - 1 versus rest
  CIagreement2iF <- CIagreement(data,cat1="2", cat2="1",correction ="Fleis" ) #negative agreement/agreement 2 vs 1

  BCIagreement0i <- CIbootagreement(data) #overall agreement
  BCIagreement1i <- CIbootagreement(data,cat1="1")
  BCIagreement2i <- CIbootagreement(data,cat1="2", cat2="1")

  CIagreement0 <- rbind(CIagreement0, CIagreement0i)
  CIagreement1 <- rbind(CIagreement1, CIagreement1i)
  CIagreement2 <- rbind(CIagreement2, CIagreement2i)
  CIagreement0F <- rbind(CIagreement0F, CIagreement0iF)
  CIagreement1F <- rbind(CIagreement1F, CIagreement1iF)
  CIagreement2F <- rbind(CIagreement2F, CIagreement2iF)
  BCIagreement0 <- rbind(BCIagreement0, BCIagreement0i)
  BCIagreement1 <- rbind(BCIagreement1, BCIagreement1i)
  BCIagreement2 <- rbind(BCIagreement2, BCIagreement2i)
}
  AGR0out <- c(colMeans(CIagreement0))
  AGR1out <- c(colMeans(CIagreement1))
  AGR2out <- c(colMeans(CIagreement2))
  AGR0outF <- c(colMeans(CIagreement0F))
  AGR1outF <- c(colMeans(CIagreement1F))
  AGR2outF <- c(colMeans(CIagreement2F))
  BAGR0out <- c(colMeans(BCIagreement0))
  BAGR1out <- c(colMeans(BCIagreement1))
  BAGR2out <- c(colMeans(BCIagreement2))
output <- rbind(AGR0out, AGR0outF, BAGR0out,
                AGR1out, AGR1outF, BAGR1out,
                AGR2out, AGR2outF, BAGR2out)
colnames(output) <- c("95%low", "Percentage", "95%high")
print(output)
}


