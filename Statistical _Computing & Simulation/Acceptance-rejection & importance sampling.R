
# Simulate N(0,1) by Acceptance-rejection method----------------------------------
set.seed(1)
n <- 10000
u <- runif(n)
exp_rv <- -log(u)
u_judge <- runif(n)
x <- numeric(0)
x <- exp_rv[u_judge <= exp(-(1/2)*(exp_rv-1)^2)]
u_judge_sign <- runif(length(x))
u_judge_sign <- ifelse(u_judge_sign<=0.5,-1,1)
x <- x*u_judge_sign
hist(x)
##mean(x)
##-0.01319339
##var(x)
##1.018131



# Importance sampling -----------------------------------------------------
# Calculate E(X) X~N(0,1) by using importance sampling with proposal distribution t(3)
I1 <- numeric(0)
for(i in 1:10){  
  t <- rt(n,df=3)                 #sampling from t(3)
  I1[i] <- mean(t*dnorm(t)/dt(t, df=3))
}
##mean(I1)
##-0.01608817

# Calculate E(X) X~t(3) by using importance sampling with proposal distribution N(0,1)

I2 <- numeric(0)
for(i in 1:10){
  n_rv <- rnorm(n)
  I2[i] <- mean(n_rv*dt(n_rv, df=3)/dnorm(n_rv))
}
#mean(I2)
#0.01009753

