
# p10 ex3 -----------------------------------------------------------------------
f <- function(x){
  return(x^2-2*x+1)
}

der_f <- function(x){
  return(2*x-2)
} 

x <- 2
y <- x-f(x)/der_f(x)
temp <- 0

while(abs(y-x)>10^(-6)){
  temp <- y-f(y)/der_f(y)
  x <- y
  y <- temp
# print(temp)
}
# y=1.0000009536 



# p23 ex ------------------------------------------------------------------
## Newton Method
f2 <- function(x){
  return(3*x^2-4*x)
}

der_f2 <- function(x){
  return(6*x-4)
}

x <- 0.01
y <- x-f2(x)/der_f2(x)
temp <- 0

while(abs(y-x)>10^(-6)){
  temp <- y-f2(y)/der_f2(y)
  x <- y
  y <- temp
 print(temp)
}
#initial x=2 converge to 1.3333333
#initial x=0.01 converge to  -1.417701e-17 because x meet 0 faster so stop


## gradient descent
f3 <- function(x){
  return(x^3-2*x^2+2)
}
der_f3 <- function(x){
  return(3*x^2-4*x)
}

x <-2
y <- x-0.1*der_f3(x)
temp <- 0
while(abs(y-x)>10^(-6)){
  temp <- y-0.1*der_f3(y)
  x <- y
  y <- temp
 print(temp)
}

# x=1.33333213873712 has local minimum
# initial=2 converge faster