# CI via bootstrap versus CI van p value
# CI van p-value = 1.96*(sqrt(1/n*p(1-p)))
# sample size correction check for CI calculation
#Riekie BMJ: 2a/2a+b+c voor positive agreement
library(Agree)

nsim <-500
n <- 100
prev1 <- c(30,70)
prev2 <- c(10,90)
prev3 <- c(20,80)
prev4 <- c(50,50)
preva <-rbind(prev1,prev2,prev3,prev4)
nlikert <- 2

ratervar <- c(8)
#degree of agreement between raters
transmat1 <- matrix(0.20, nlikert,nlikert, byrow=TRUE)
diag(transmat1) <- 1-((nlikert-1)*0.20)
y <- 1
nrat <- ratervar[y]
for (v in 1:4){
  prev <- preva[v,]
  PCIs <- FCIs <- PCIa1s <- FCIa1s <- PCIa2s <- FCIa2s <- vector()
  BCIs <- BCInegs <- BCIposs <- vector()
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

    dat <- apply(ratersy, 2, as.factor)
    dat <- data.frame(dat)
    raters <- paste("R", 1:nraters, sep="")
    variable <- c("var")
    colnames(dat) <-paste(raters, variable, sep="_")
    ratersvars <-  paste(raters,variable, sep = "_")
    sumtab <- sumtable(dat,ratersvars,levels=1:nlikert)

    ag1 <-agreement(sumtab)
    agree1 <- specific.agreement1(sumtab, "positive")
    agree2 <- specific.agreement1(sumtab, "negative")

    Nc <- nrow(data)*sqrt(nraters-1)
    Nsp <-((sumtab[1,1]+sumtab[1,2])/(nraters*(nraters-1)/2))*sqrt(nraters-1)
    Nsn <-((sumtab[2,2]+sumtab[1,2])/(nraters*(nraters-1)/2))*sqrt(nraters-1)

    PCI <- CIagreement(ag1,m=nraters,n=nrow(data))
    FCI <- CIagreement(ag1,m=nraters,n=nrow(data),correction="Fleis")

    PCIa1<- CIagreement(agree1,m=nraters,n=((sumtab[1,1]+sumtab[1,2])/(nraters*(nraters-1)/2)))
    FCIa1<- CIagreement(agree1,m=nraters,n=((sumtab[1,1]+sumtab[1,2])/(nraters*(nraters-1)/2)), correction="Fleis")

    PCIa2<- CIagreement(agree2,m=nraters,n=((sumtab[2,2]+sumtab[1,2])/(nraters*(nraters-1)/2)))
    FCIa2<- CIagreement(agree2,m=nraters,n=((sumtab[2,2]+sumtab[1,2])/(nraters*(nraters-1)/2)), correction="Fleis")

    PCIs <- rbind(PCIs ,PCI)
    FCIs <- rbind(FCIs, FCI)
    PCIa1s  <- rbind(PCIa1s, PCIa1)
    FCIa1s <- rbind(FCIa1s, FCIa1)
    PCIa2s  <- rbind(PCIa2s, PCIa2)
    FCIa2s <- rbind(FCIa2s, FCIa2)

    library(boot)
    agree.boot <- function(data,x) {agreement(sumtable(data[x,],ratings=colnames(data),levels=1:nlikert))}
    res1a <- boot(dat,agree.boot,500)
    BCI_agr <-  quantile(res1a$t,c(0.025,0.975))    # Bootstrapped confidence interval of Light's kappa
    BCI_agree <- BCI_agr    #,BCIadj_agr$bca[4:5])
    BCI <- c(BCIlow=BCI_agree[1], est=ag1, BCIhigh=BCI_agree[2])
    BCIs <- rbind(BCIs,BCI)

    agree.bootp <- function(data,x) {specific.agreement1(sumtable(data[x,],ratings=colnames(data), levels=1:nlikert),specific = "positive" )}
    res1a <- boot(dat,agree.bootp,500)
    BCI_agrpos <-  quantile(res1a$t,c(0.025,0.975))    # Bootstrapped confidence interval of Light's kappa
    BCI_agreepos <- BCI_agrpos   #,BCIadj_agr$bca[4:5])
    BCIpos <- c(BCIlow=BCI_agreepos[1], est=agree1, BCIhigh=BCI_agreepos[2])
    BCIposs <- rbind(BCIposs,BCIpos)

    agree.bootn <- function(data,x) {specific.agreement1(sumtable(data[x,],ratings=colnames(data), levels=1:nlikert),specific = "negative" )}
    res1a <- boot(dat,agree.bootn,500)
    BCI_agrneg <-  quantile(res1a$t,c(0.025,0.975))    # Bootstrapped confidence interval of Light's kappa
    BCI_agreeneg <- BCI_agrneg    #,BCIadj_agr$bca[4:5])
    BCIneg <- c(BCIlow=BCI_agreeneg[1], est=agree2, BCIhigh=BCI_agreeneg[2])
    BCInegs <- rbind(BCInegs,BCIneg)

  }

  PCIout <- colMeans(cbind(PCIs, PCIs-BCIs, (PCIs-BCIs)^2))
  FCIout <- colMeans(cbind(FCIs, FCIs-BCIs, (FCIs-BCIs)^2))
  PCIa1out <- colMeans(cbind(PCIa1s, PCIa1s-BCIposs,(PCIa1s-BCIposs)^2))
  FCIa1out <- colMeans(cbind(FCIa1s, FCIa1s-BCIposs,(FCIa1s-BCIposs)^2))
  PCIa2out <- colMeans(cbind(PCIa2s, PCIa2s-BCInegs,(PCIa2s-BCInegs)^2))
  FCIa2out <- colMeans(cbind(FCIa2s, FCIa2s-BCInegs,(FCIa2s-BCInegs)^2))
  BCIout <- c(colMeans(BCIs), rep(NA,6))
  BCIa1out <- c(colMeans(BCIposs), rep(NA,6))
  BCIa2out <- c(colMeans(BCInegs), rep(NA,6))


  output <- rbind(PCIout, FCIout, BCIout,PCIa1out, FCIa1out, BCIa1out,PCIa2out,FCIa2out, BCIa2out)
  rownames(output) <- c("PCI","FCI","BCI","PCIpos","FCIpos","BCIpos","PCIneg","FCIneg","BCIneg")
  colnames(output) <- c("95%low", "Percentage", "95%high","95%low_bias", "Percentage_bias", "95%high_bias","95%low_MSE", "Percentage_MSE", "95%high_MSE")

 # write.table(output, paste0("outputagee80spec_prev", preva[v],"raters",nrat,".txt"), dec=",", sep="\t")
  print(output)
}


