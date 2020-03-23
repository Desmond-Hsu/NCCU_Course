# Importance Sampling -----------------------------------------------------
size <- 1000
u  <- dunif(x1, min=-20, max=20)
f1 <- c()
f2 <- c()
f_mc <- c()

for(i in 1:5000){
  x1 <- rnorm(size, mean=0, sd=1)
  f1[i] <- mean(40*exp(-abs(x1))*u/dnorm(x1))
}
for(i in 1:5000){
  x2 <- rnorm(size, mean=0, sd=sqrt(5))
  f2[i] <- mean(40*exp(-abs(x2))*u/dnorm(x2, mean=0, sd=sqrt(5)))
}



## Monte Carlo
for(i in 1:5000){
  uni <- runif(size,-20,20)
  f_mc[i] <- mean(40*exp(-abs(uni)))
}

ans1 <- mean(f1)
ans1_var <- var(f1)
ans2 <- mean(f2)
ans2_var <- var(f2)
u_mc <- mean(f_mc)
u_var <- var(f_mc)
# ans1
#[1] 1.990377
# ans1_var
#[1] 0.03692943
# ans2
#[1] 1.998728
# ans2_var
#[1] 0.001909767  g(x)= N(0,5) has the smallest variance
# u_mc
#[1] 2.001757
# u_var
#[1] 0.03562908

# plot
par(mfrow=c(1,3))
hist(f1,breaks=30,xlim=c(1,3))
hist(f2,breaks=20,xlim = c(1,3))
hist(f_mc, breaks = 20, xlim=c(1,3))


# P(X>3), X~N(0,1) --------------------------------------------------------
n <- 100
I1 <- numeric(0)
I2 <- numeric(0)
##(1)
for(i in 1:100000){
  x <- rnorm(n)
  judge <- x>3
  I1[i] <- mean(judge)
}
##(2)
for(i in 1 :100000){
  y <- rnorm(n, mean=4, sd=1)
  g <- y[y>3]
  I2 <- dnorm(g)/dnorm(g, mean=4, sd=1)
}

ans_I1 <- mean(I1)
var_I1 <- var(I1)
ans_I2 <- mean(I2)
var_I2 <- var(I2)
# ans_I1
#[1] 0.0013583
# var_I1
#[1] 1.357416e-05
# ans_I2
#[1] 0.001301785
# var_I2
#[1] 8.369691e-06
CI1 <-  c(ans_I1-1.96*sqrt(var_I1),ans_I1+1.96*sqrt(var_I1))
CI2 <-  c(ans_I2-1.96*sqrt(var_I2),ans_I2+1.96*sqrt(var_I2))
# CI1
#[1] -0.005862952  0.008579552
# CI2
#[1] -0.004368577  0.006972147