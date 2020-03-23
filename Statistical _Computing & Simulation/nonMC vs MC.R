
# Non Monte Carlo ---------------------------------------------------------
n <- 5^5 
v <- 10^5
a <- c(-3,-1,1,3,5)
hf <- rep(0,n)

k <- 0
for(i1 in 1:5){
  for(i2 in 1:5){
    for(i3 in 1:5){
      for(i4 in 1:5){
        for(i5 in 1:5){
          k <- k+1
          hf[k] <- (a[i1]*a[i2]*a[i3]*a[i4]*a[i5])^2*(dnorm(a[i1])*dnorm(a[i2])*dnorm(a[i3])*dnorm(a[i4])*dnorm(a[i5]))
        }
      }
    }
  }
}

l1 <- v*mean(hf)
#l1
#[1] 1.822175
#big variance, depends on value of a

# Monte Carlo -------------------------------------------------------------

h <- rep(0,n)
for(i in 1:n){
  h[i] <- (rnorm(1)*rnorm(1)*rnorm(1)*rnorm(1)*rnorm(1))^2
}
l2 <- mean(h)
#l2
#[1] 0.9484435