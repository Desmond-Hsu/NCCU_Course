# numeric p25
f2 <- function(x){
  return(3*x^2-4*x)
}

der_f2 <- function(x){
  return(6*x-4)
}



# Newton Method
Newton <- function(x,fx,der_fx,tolerance,der_2){
  i <- 0
  y <- x-fx(x)/der_fx(x)
  while(abs(y-x)>tolerance){
    temp <- y-fx(y)/der_fx(y)
    x <- y
    y <- temp
    i = i+1
    if(i>50){break}
  }
  print(c(i,y))
}



# Gradient Descent
Gradient <- function(x,fx,r,tolerance){
  y <- x-r*fx(x)
  i <- 0
  while(abs(y-x)>tolerance){
    temp <- y-r*fx(y)
    x <- y
    y <- temp
    i = i+1
    if(i>50){break}
  }
  print(c(i,y))
}


Newton(2,f2,der_f2,10^(-6))    #[1] 4.000000 1.333333
Gradient(2,f2,0.1,10^(-6))     #[1] 24.000000  1.333334
Newton(0.01,f2,der_f2,10^(-6)) #[1]  2.000000e+00 -1.417701e-17
Gradient(0.01,f2,0.1,10^(-6))  #[1] 41.000000  1.333332
