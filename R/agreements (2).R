agreements <- function(dat, nlikert, crosval) {
  crostab <- list()
  source('R/FleisCI.R')
  source('R/ProbCI.R')
  nraters <- ncol(dat)
  for (i in 1:nrow(crosval)) {
    v1 <- dat[crosval[i,1]][,1]
    v1.1 <- factor(v1, levels = 1:nlikert)
    v2 <- dat[crosval[i,2]][,1]
    v2.1 <- factor(v2, levels = 1:nlikert)
    table  <- ftable(v1.1, v2.1)
    crostab[[i]] <- table
    if (i == 1) {
      sumtab <- crostab [[i]]
    }
    if (i > 1) {
      sumtab <- sumtab + crostab[[i]]
    }
  }

  agree_overall <- sum(diag(sumtab)) / sum(sumtab)
  PCI2_agree_overall <- ProbCI(p=agree_overall, n=nrow(dat)*sqrt(nraters-1))
  FCI2_agree_overall <- FleisCI(p=agree_overall, n=nrow(dat)*sqrt(nraters-1))
  ## overeenkomst met een afwijking
  agree_pos <- ((2*(sumtab[1,1]))/((2*(sumtab[1,1]))+(sumtab[1,2])+(sumtab[2,1])))
  PCI2_agree_pos <- ProbCI(p=agree_pos, n=((sumtab[1,1]+sumtab[1,2])/(nraters*(nraters-1)/2))*sqrt(nraters-1))
  FCI2_agree_pos <- FleisCI(p=agree_pos, n=((sumtab[1,1]+sumtab[1,2])/(nraters*(nraters-1)/2))*sqrt(nraters-1))
  agree_neg <- ((2*(sumtab[2,2]))/((2*(sumtab[2,2]))+(sumtab[1,2])+(sumtab[2,1])))
  PCI2_agree_neg <- ProbCI(p=agree_neg, n=((sumtab[2,2]+sumtab[1,2])/(nraters*(nraters-1)/2))*sqrt(nraters-1))
  FCI2_agree_neg <- FleisCI(p=agree_neg, n=((sumtab[2,2]+sumtab[1,2])/(nraters*(nraters-1)/2))*sqrt(nraters-1))

  agreeup <- agreelow <- vector()
  for (i in 1:(nlikert - 1)) {
    agreeup[i] <- sumtab[i,i + 1]
    agreelow[i] <- sumtab[i + 1,i]
  }
  agree_overall2 <-
    sum(diag(sumtab),agreeup, agreelow) / sum(sumtab)
  PCI2_agree_overall2 <- ProbCI(p=agree_overall2, n=nrow(dat)*sqrt(nraters-1))
  FCI2_agree_overall2 <- FleisCI(p=agree_overall2, n=nrow(dat)*sqrt(nraters-1))

  ## off diagonal means in matrix
  mat1 <- matrix(0,nlikert,nlikert)
  for (i in 1:nlikert) {
    for (j in 1:nlikert) {
      mat1[i,j] <- (sumtab[i,j] + sumtab[j,i]) / 2
      mat1[j,i] <- (sumtab[i,j] + sumtab[j,i]) / 2
    }
  }
  mat1_rowtot <- rowSums(mat1)
  mat1.2 <- cbind(mat1,mat1_rowtot)
  mat1.2_coltot <- colSums(mat1.2)
  mat1.2_colperc <- (mat1.2_coltot / mat1.2_coltot[nlikert + 1]) * 100
  betweentable <-
    rbind(mat1.2, c(mat1.2_colperc[1:nlikert],mat1.2_coltot[nlikert + 1]))
  rownames(betweentable) <- c(paste(1:nlikert), "PERC")
  colnames(betweentable) <- c(paste(1:nlikert), "SUM")
  agree_pcat <- mat1.2_colperc[1:nlikert]


  mat2 <- matrix (0,nlikert,nlikert)
  for (i in 1:nlikert) {
    mat2[i,] <- mat1[i,] / mat1_rowtot[i]
  }
  perc_agree_cat <- round(mat2 * 100,2)
  rownames(perc_agree_cat) <- c(paste(1:nlikert))
  colnames(perc_agree_cat) <- c(paste(1:nlikert))

  #LR ratio
  # pre-odds from prob between table last row
  # post odds from prob agree_pcat table

  preodds <- 1 / (1 - (agree_pcat / 100))
  postodds <- 1 / (1 - (perc_agree_cat / 100))
  LRtable <- sweep(postodds,2,preodds,"/")

  list(
    agree_overall = agree_overall,
    PCI2_agree_overall=PCI2_agree_overall,
    FCI2_agree_overall=FCI2_agree_overall,
    sumtab = sumtab,
    agree_overall2 = agree_overall2,
    PCI2_agree_overall2=PCI2_agree_overall2,
    FCI2_agree_overall2=FCI2_agree_overall2,
    agree_pos = agree_pos,
    PCI2_agree_pos = PCI2_agree_pos,
    FCI2_agree_pos = FCI2_agree_pos,
    agree_neg = agree_neg,
    PCI2_agree_neg = PCI2_agree_neg,
    FCI2_agree_neg = FCI2_agree_neg,
    betweentable = betweentable,
    agree_pcat = agree_pcat,
    perc_agree_cat = perc_agree_cat,
    LRtable =  LRtable,
    mat1 = mat1
  )
}
