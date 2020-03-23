
z <- rnorm(n=500)
x1 <- ((1/500)^(1/2))*z
y1 <- cumsum(x1)
x_point <- seq(0,1,by=1/500)
x_point <- x_point[-1]
x11()
plot(x=x_point ,y=y1,type='l',main='Simulating Standard Brownian process',
     xlab = 'time', ylab = 'number of events')

x2 <- sqrt(2)*sqrt(1/500)*z+(5/500)
y2 <- cumsum(x2)
x11()
plot(x=x_point ,y=y2,type='l',main='Simulating Brownian process',
     xlab = 'time', ylab = 'number of events')