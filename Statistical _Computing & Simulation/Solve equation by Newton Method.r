install.packages("numDeriv")
library(numDeriv)

Newton <- function(x,f,tolerance){
  judge <- 1
  i=0
  while(max(judge)>tolerance){
    Jac <- jacobian(func = f,x)
    fx_mat <- matrix(f(x), nrow = 3)
    y <- x- c(solve(Jac)%*%fx_mat)
    judge <- abs(y-x)
    x <- y
    i=i+1
  }
  print(x)
  sprintf('%d times iterates',i)
}

x1 <- c(0.1,0.1,-0.1)
f <- function(x){
  f1 <- 3*x[1] - cos(x[2]*x[3]) - (1/2)
  f2 <- x[1]^2 - 81*(x[2]+0.1)^2 + sin(x[3])+1.06
  f3 <- exp(-x[1]*x[2]) +  20*x[3] + (10*pi-3)/3
  c(f1,f2,f3)
}

x2 <- c(1,1,-1)
g <- function(x){
  f1 <- 3*x[1] - cos(x[2]*x[3]) - (1/2)
  f2 <- x[1]^2 - 652*x[2]^2 - (1/4)
  f3 <- exp(-x[1]*x[2]) +  20*x[3] + (10*pi-3)/3
  c(f1,f2,f3)
}

# Newton(x1,f,10^-6)
# [1]  5.000000e-01  1.422309e-18 -5.235988e-01
# [1] "5 times iterates"

# Newton(x2,g,10^-6)
# [1]  5.000000e-01  9.535794e-07 -5.235988e-01
# [1] "20 times iterates"








