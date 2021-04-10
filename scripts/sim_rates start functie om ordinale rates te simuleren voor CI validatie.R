## Simulatie script voor data om CI te valideren in een functie vangen.
## to do: functie begrijpen en versnellern (loops eruit)
## dan draaien om werkelijk CI's te checken.


#degree of agreement between raters
transmat1 <- matrix(0.20, nlikert,nlikert, byrow=TRUE)
diag(transmat1) <- 1-((nlikert-1)*0.20)
y <- 1
nrat <- ratervar[y]



k <- 3
n <- 100
prev <- c(0.3 , 0.2, 0.5) #should sum to one; prevalence per category.
agreement <- 0.7
sim_rates <- function(k, n, prev, agreement)

ncat <- length(prev)
x <- array(rnorm(k*n), dim = c(n, k)) #normally distributed variables for each k of length n
M <- matrix(0.6,k,k) #correlation matrix
diag(M) <- 1

# adjust correlations for uniforms
for (i in 1:k){
  for (j in max(i, 2):k){
    if (i != j){
      M[i, j] <- 2 * sin(pi * M[i, j] / 6)
      M[j, i] <- 2 * sin(pi * M[j, i] / 6)
    }
  }
}
# induce correlation in x, check correlations
C <- chol(M)
y <- x %*% C
#cor(y)
# create uniforms, check correlations
Y <- pnorm(y)
cor(Y)

#use prevalence to cut uniforms.
p_prev <- (prev)/sum((prev))
s_prev <- cumsum(p_prev)
res <- outer(runif(n), s_prev, ">")
true <- rowSums(res)+1


## HIER GEBLEVEN >> de transmatrix lijkt verkeerd te gaan bij meer dan 2 categorieen? Klopt dat? Of hoort dit zo te gaan? Proberen te achterhalen uit oude files.
s_trans <- matrix(0,ncat,ncat)
transmat <- matrix(agreement, ncat,ncat, byrow=TRUE)
diag(transmat) <- 1-((ncat-1)*agreement)

apply(transmat, 1, cumsum)

for(c in 1:nrow(transmat)){s_trans[c,] <- cumsum(transmat[c,])}
ratersy <- Y
for (r in 1:ncol(ratersy)){
  for (k in 1:ncol(s_trans)){
    ratersy[,r][ratersy[,r]<s_trans[true,k]] <- k
  }
}

dat <- apply(ratersy, 2, as.factor)
dat <- data.frame(dat)
raters <- paste("R", 1:k, sep="")
variable <- c("var")
colnames(dat) <-paste(raters, variable, sep="_")
ratersvars <-  paste(raters,variable, sep = "_")
