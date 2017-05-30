
FleisCI <- function (p, n, a=0.05){
  a1 <- qnorm(1-(a/2))
  FCIlow <- ((2*n*p+(a1*a1)-1)-a1*sqrt((a1*a1)-(2+(1/n))+4*p*(n*(1-p)+1)))/(2*((a1*a1)+n))
  FCIhigh <- ((2*n*p+(a1*a1)-1)+a1*sqrt((a1*a1)-(2+(1/n))+4*p*(n*(1-p)+1)))/(2*((a1*a1)+n))  
  c(CIlow=FCIlow, p=p, CIhigh=FCIhigh)
}

