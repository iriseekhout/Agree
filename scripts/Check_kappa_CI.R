df <-  data.frame(r1=factor(c(1,0,1,0,0,1,1,0,0,0,1,1,0,1,1)),
                  r2=factor(c(1,1,1,1,0,1,1,0,0,0,1,1,0,1,0)),
                  r3=factor(c(1,1,1,0,0,0,1,1,1,0,0,1,0,1,1)),
                  r4=factor(c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1)))
 table <- sumtable(df=df, ratings=c("r1", "r2", "r3", "r4"), levels=c("0","1"))
 kappa(df)
 kappa(table)
 kappa(df , confint = TRUE)
 kappa(table, confint = TRUE)

 bootCI(data = df, fun = kappa)

 psych::cohen.kappa(df)

 psych::cohen.kappa(df1)
 psych::cohen.kappa(table)
 psych::wkappa(df)
 psych::wkappa(table)


 #2 raters to check CI's
 ## CI is the same with psych check
 df1 <- df[,1:2]
 kappa(df1, confint = TRUE)
 cohen.kappa(df1)

 ## for multiple raters; n assumption check.
 kappa(table, k = 4, n = 15, confint = TRUE)
 cohen.kappa(table)

 bootCI(data = df, fun = kappa)

 df <- data.frame(r1=factor(c(1,2,2,0,3,3,1,0,3,0,2,2,0,3,1)),
                   r2=factor(c(1,1,1,0,3,3,1,0,1,0,2,2,0,2,1)),
                   r3=factor(c(1,1,1,3,3,2,1,0,1,0,2,2,0,3,1)),
                   r4=factor(c(1,2,1,0,3,3,1,0,3,0,2,2,0,2,1)))
  table <- sumtable(df=df, ratings=c("r1", "r2", "r3", "r4"), levels=c("0","1", "2", "3"))
  kappa(df)
  kappa(table)
  kappa(df, confint = TRUE)
  kappa(table)
  cohen.kappa(table)
  bootCI(df, fun = kappa)

  df1 <- df[,1:2]
  kappa(df1, confint = TRUE)
  table <- sumtable(df1)
  kappa(table)
  cohen.kappa(table)
  bootCI(df1, fun = kappa)
