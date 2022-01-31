df1 <- data.frame(r1=factor(c(1,2,2,0,3,3,1,0,3,0,2,2,0,3,1)),
                 r2=factor(c(1,1,1,0,3,3,1,0,1,0,2,2,0,2,1)),
                 r3=factor(c(1,1,1,3,3,2,1,0,1,0,2,2,0,3,1)),
                 r4=factor(c(1,2,1,0,2,2,1,0,2,0,2,2,0,2,1)))


df2 <- cbind(r1=c(1,2,2,0,3,3,1,0,3,0,2,2,0,3,1),
                 r2=c(1,1,1,0,3,3,1,0,1,0,2,2,0,2,1),
                 r3=c(1,1,1,3,3,2,1,0,1,0,2,2,0,3,1),
                 r4=c(1,2,1,0,2,2,1,0,2,0,2,2,0,2,1))

df3 <- tibble::as_tibble(df2)

sumtable(df1)
sumtable(df2)
sumtable(df3)

agreement(df1)
agreement(df2)
agreement(df3)

df1 <- data.frame(r1 = c("a" ,"b" ,"c" ,"a", "a", "c", "a", "b", "c", "a", "b", "c", "b"),
                  r2 = c("a" ,"b" ,"b" ,"b", "a", "b", "b", "a", "b", "b", "a", "a", "b"),
                  r3 = c("c" ,"a" ,"b" ,"c", "a", "b", "c", "a", "b", "c", "a", "b", "c"),
                  r4 = c("b" ,"c" ,"a" ,"c", "b", "c", "a", "c", "b", "a", "c", "b", "a"))

df2 <- data.frame(r1 = factor(c("a" ,"b" ,"c" ,"a", "a", "c", "a", "b", "c", "a", "b", "c", "b")),
                  r2 = factor(c("a" ,"b" ,"b" ,"b", "a", "b", "b", "a", "b", "b", "a", "a", "b")),
                  r3 = factor(c("c" ,"a" ,"b" ,"c", "a", "b", "c", "a", "b", "c", "a", "b", "c")),
                  r4 = factor(c("b" ,"c" ,"a" ,"c", "b", "c", "a", "c", "b", "a", "c", "b", "a")))

sumtable(df1)
sumtable(df2)

agreement(df1)
agreement(df2)
