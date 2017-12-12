Sanna

Legenda
0	Geen BCC
1	Wel BCC
9	Missing values



Legenda
0	Geen BCC
1	superficieel BCC
2	nodulair BCC
3	aggressief BCC
9	Missing values

library(xlsx)
dat1 <- read.xlsx("C:/Users/eekhouti/CloudStation/VUmc/Agreement/Sanna/Database reliability studie.xlsx", sheetIndex = 1)
dat2 <- read.xlsx("C:/Users/eekhouti/CloudStation/VUmc/Agreement/Sanna/Database reliability studie.xlsx", sheetIndex = 2)


dat1[dat1==9] <- NA
dat2[dat2==9] <- NA

dat1[dat1==0] <- "Geen BCC"
dat1[dat1==1] <- "Wel BCC"

dat1 <- data.frame(lapply(dat1,as.factor))

atable1.1 <- sumtable(dat1, ratings = c("DK1", "YE1", "MP1"), offdiag = FALSE)
atable1.2 <- sumtable(dat1, ratings = c("DK2", "YE2", "MP2"), offdiag = FALSE)

agreement(atable1.1)
agreement(atable1.2)

CIagreement(agreement(atable1.1), n=nrow(dat1), m=3)
CIagreement(agreement(atable1.2), n=nrow(dat1), m=3)

#specific agreements

