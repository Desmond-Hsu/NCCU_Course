library(dplyr)
library(ggplot2)
library(mvtnorm)

x <- faithful

#initial parameter
alpha1 = c(0.5)
alpha2 = c(0.5)
mu1 = matrix(c(2, 55), ncol = 1)
mu2 = matrix(c(4.4, 80), ncol = 1)
sigma1 = matrix(c(0.8, 7, 7, 70), ncol = 2)
sigma2 = matrix(c(0.8, 7, 7, 70), ncol = 2)

theta <- list()
theta[[1]] <- list(alpha1 = alpha1, 
                   alpha2 = alpha2, 
                   mu1 = mu1,
                   sigma1 = sigma1,
                   mu2 = mu2, 
                   sigma2 = sigma2)
EM = function(x, parameter){
  p_group <- function(y, x, parameter){
    a <- parameter$alpha1*dmvnorm(x, parameter$mu1, parameter$sigma1)
    b <- parameter$alpha2*dmvnorm(x, parameter$mu2, parameter$sigma2)
    if(y == 1){
      z <- a/(a+b)
    }else{
      z <- b/(a+b)
    }
    return(z)
  }

  #E step
  Q <- function(x, parameter, n = nrow(x)){
    a <- parameter$alpha1*dmvnorm(x, parameter$mu1, parameter$sigma1)
    b <- parameter$alpha2*dmvnorm(x, parameter$mu2, parameter$sigma2)
  
    z <- log(a)*p_group(1, x, parameter) + log(b)*p_group(2, x, parameter)
    return(sum(z))
  }
  Q(x, theta[[1]])

  #M step
  M_step <- function(x, parameter){
    alpha1_h <- sum(p_group(1, x, parameter))/nrow(x)
    alpha2_h <- sum(p_group(2, x, parameter))/nrow(x)
    mu1_h <- sapply(x*p_group(1, x, parameter), sum)/sum(p_group(1, x, parameter))
    mu2_h <- sapply(x*p_group(2, x, parameter), sum)/sum(p_group(2, x, parameter))
    new_sigma <- function(y, x, parameter){
      if(y == 1){
        mu <- parameter$mu1
      }else{
        mu <- parameter$mu2
      }
      a11 <- sum((x[,1]-mu[1])^2*p_group(y, x, parameter))/sum(p_group(y, x, parameter))
      a12 <- sum((x[,1]-mu[1])*(x[,2]-mu[2])*p_group(y, x, parameter))/sum(p_group(y, x, parameter))
      a22 <- sum((x[,2]-mu[2])^2*p_group(y, x, parameter))/sum(p_group(y, x, parameter))
      z <- matrix(c(a11, a12, a12, a22), ncol = 2)
      return(z)
    }
    sigma1_h <- new_sigma(1, x, parameter)
    sigma2_h <- new_sigma(2, x, parameter)
    new_theta <- list(alpha1 = alpha1_h, 
                      alpha2 = alpha2_h, 
                      mu1 = mu1_h,
                      sigma1 = sigma1_h,
                      mu2 = mu2_h, 
                      sigma2 = sigma2_h)
    return(new_theta)
  }

  #EM Model
  n <- 0
  for (i in 1:50) {
    theta[[i+1]] <- M_step(x, theta[[i]])
    if(Q(x, theta[[i]]) >= Q(x, theta[[i+1]])) break
    n = n + 1
  }
  print(n)
  #final parameter
  print(theta[[n]])

  #plot 
  x$group <- ifelse(p_group(1, x, theta[[n]]) >= 0.5, 1, 5)
  ggplot(x, aes(x = x$eruptions, y = x$waiting)) +
  geom_point(colour = as.factor(x$group))
}
