library(gtools)
x <- as.matrix(faithful$eruptions)

alpha0=c(0.5,0.5)
mu0=c(2,4)
tau0=c(10/8,10/8)

normal0 <- array(rep(c(0,1)),dim=c(2,2))
gamma0 <- c(1,1)
dirichlet0 <- c(1,1)
#sampling_y##
sampling_y <- function(x,mu, tau, alpha){
  prob1 <- alpha[1]*dnorm(x,mean=mu[1],sd=tau[1]^(-1/2))
  prob2 <- alpha[2]*dnorm(x,mean=mu[2],sd=tau[2]^(-1/2))
  p1 <- prob1/(prob1+prob2)
  p2 <- 1-p1
  y_matrix <- array(dim = c(272,2))
  for(i in 1:length(x)){
    y_matrix[i,] <- rmultinom(n=1, size=1, prob=c(p1[i],p2[i]))
  }
  return(y_matrix)  
}
result <- c()
#renew parameter
Gibbs_sampling <- function(x,mu0,tau0,alpha0,noramal0,gamma0,dirichlet0){
mu <- as.data.frame(mu0)
tau <- as.data.frame(tau0)
alpha <- as.data.frame(alpha0)
  for(i in 1:1000){
  y <- sampling_y(x,mu[,i],tau[,i],alpha[,i])
  n1 <- sum(y[,1])
  n2 <- sum(y[,2])
  x_bar <- c((t(x)%*%y)[1]/n1, (t(x)%*%y)[2]/n2)
  mu_bar <- c((normal0[1,1]*normal0[2,1]+n1*tau[1,i]*x_bar[1])/(normal0[2,1]+n1*tau[1,i]),
              (normal0[1,2]*normal0[2,2]+n2*tau[2,i]*x_bar[2])/(normal0[2,2]+n2*tau[2,i]))
  tau_bar <- c(normal0[2,1]+n1*tau[1,i], normal0[2,2]+n2*tau[2,i])
  mu_new <- c(rnorm(1,mean=mu_bar[1], sd=tau_bar[1]^(-1/2)), 
              rnorm(1,mean=mu_bar[2], sd=tau_bar[2]^(-1/2)))
  tau_new <- c(rgamma(1,gamma0[1]+n1/2, rate=gamma0+((x_bar[1]-mu[1,i])^2)/2),
               rgamma(1,gamma0[2]+n2/2, rate=gamma0+((x_bar[2]-mu[2,i])^2)/2))
  alpha_new <- rdirichlet(1,c(dirichlet0[1]+n1, dirichlet0[2]+n2))
  mu <- cbind(mu,mu_new)
  tau<- cbind(tau,tau_new)
  alpha <- cbind(alpha,t(alpha_new))
  }
colnames(mu) <- rep(c(0:1000))
colnames(tau) <- rep(c(0:1000))
colnames(alpha) <- rep(c(0:1000))
result <- rbind(mu,tau)
result <- rbind(result,alpha)
# colnames(result) <- c(0:1000)
rownames(result) <- c('mu1','mu2','tau1','tau2','alpha1','alpha2')
View(result)
}

Gibbs_sampling(x,mu0,tau0,alpha0,noramal0,gamma0,dirichlet0)
