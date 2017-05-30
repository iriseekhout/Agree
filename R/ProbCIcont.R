ProbCIcont <- function(p,n,a=0.05){
  p+c(-(qnorm(1-(a/2))*(sqrt(1/n*(p*(1-p))))-1/(2*n)),0, (qnorm(1-(a/2))*(sqrt(1/n*(p*(1-p))))+1/(2*n)))
}


