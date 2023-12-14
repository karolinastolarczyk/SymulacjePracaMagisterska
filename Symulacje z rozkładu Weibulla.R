#Ustawiamy ziarno
set.seed(17)

library('plyr')

#Ustalamy parametr a 
a <- 0.8

#Ustalamy parametry a i b dla rozkładu Weibulla
a_par <- 5

#deterministyczna wielkosc probki

# funkcja pomocnicza wyznaczająca K_{n:n}(a)
generate_weibull <- function(a, N, a_par) {
  y_rweibull <- rweibull(N, a_par, 1)
  y_rweibull_max <- max(y_rweibull)
  K <- count(y_rweibull>y_rweibull_max-a)
  K_1 <-K[which(K$x == TRUE),'freq']
  res <- K_1
  return(res)
}



#generowanie dla róznych wielości próbek
generate_weibull(a, 10, a_par)
generate_weibull(a, 100, a_par)
generate_weibull(a, 1000, a_par)
generate_weibull(a, 10000, a_par)
generate_weibull(a, 100000, a_par)
generate_weibull(a, 1000000, a_par)
generate_weibull(a, 10000000, a_par)
generate_weibull(a, 100000000, a_par)
#generate_weibull(a, 1000000000, a_par)

#100

x_pweibull100 <- seq(0, 100, by = 1) 

K <- c()
for (x in 1:3001) {
  K[x] <- generate_weibull(a, 100, a_par)
} 

K
length(K)
v <- c()
for (x in x_pweibull100 ) {
  ile<- count(K<=x)
  v[x+1] <- ile[which(ile$x == TRUE),'freq']/length(K)
} 
v100<- v
v100


plot(x_pweibull100, v100, pch="_", col="blue", xlab="x", ylab="F(x)")  
legend(20, 0.2, legend=c("Dystrybuanta K "),
       col=c( "blue"), lty=1:2, cex=0.8)

#1000

x_pweibull1000 <- seq(0, 1000, by = 10) 

K <- c()
for (x in 1:3001) {
  K[x] <- generate_weibull(a, 1000, a_par)
} 

K
length(K)
v <- c()
for (x in x_pweibull1000 ) {
  ile<- count(K<=x)
  v[x/10+1] <- ile[which(ile$x == TRUE),'freq']/length(K)
} 
v1000<- v
v1000


plot(x_pweibull1000, v1000, pch="_", col="blue", xlab="x", ylab="F(x)")  
legend(200, 0.2, legend=c("Dystrybuanta K "),
       col=c( "blue"), lty=1:2, cex=0.8)

#10 000

x_pweibull10000 <- seq(0, 10000, by = 100) 

K <- c()
for (x in 1:3001) {
  K[x] <- generate_weibull(a, 10000, a_par)
} 

K
length(K)
v <- c()
for (x in x_pweibull10000 ) {
  ile<- count(K<=x)
  v[x/100+1] <- ile[which(ile$x == TRUE),'freq']/length(K)
} 
v1000<- v
v1000


plot(x_pweibull10000, v1000, pch="_", col="blue", xlab="x", ylab="F(x)")  
legend(7000, 0.2, legend=c("Dystrybuanta K "),
       col=c( "blue"), lty=1:2, cex=0.8)

#100 000

x_pweibull100000 <- seq(0, 100000, by = 1000) 

K <- c()
for (x in 1:3001) {
  K[x] <- generate_weibull(a, 100000, a_par)
} 

K
length(K)
v <- c()
for (x in x_pweibull100000 ) {
  ile<- count(K<=x)
  v[x/1000+1] <- ile[which(ile$x == TRUE),'freq']/length(K)
} 
v100000<- v
v100000
options(scipen=10000)

par(mfrow=c(1,1))
plot(x_pweibull100000, v100000, pch="_", col="blue", xlab="x", ylab="F(x)")  
legend(15000, 0.2, legend=c("Dystrybuanta K100000 "),
       col=c( "blue"), lty=1:2, cex=0.8)


attach(mtcars)

par(mfrow=c(2,2))

plot(x_pweibull100, v100, pch="_", col="blue", xlab="x", ylab="F(x)")  
legend(5, 0.8, legend=c("Dystrybuanta K100 "),
       col=c( "blue"), lty=1:2, cex=0.6)
plot(x_pweibull1000, v1000, pch="_", col="blue", xlab="x", ylab="F(x)") 
legend(50, 0.8, legend=c("Dystrybuanta K1000 "),
       col=c( "blue"), lty=1:2, cex=0.6)
plot(x_pweibull10000, v10000, pch="_", col="blue", xlab="x", ylab="F(x)")  
legend(4500, 0.4, legend=c("Dystrybuanta K10000 "),
       col=c( "blue"), lty=1:2, cex=0.6)
plot(x_pweibull100000, v100000, pch="_", col="blue", xlab="x", ylab="F(x)")  
legend(5000, 0.8, legend=c("Dystrybuanta K100000 "),
       col=c( "blue"), lty=1:2, cex=0.6)

# losowa wielkosc próbki
#N(t) - proces Poissona 
#N(t)  -> \infty

lambda <- 0.5

generate_poisson <- function(lambda, t){
  rhos <- NULL
  i <- 1
  while(sum(rhos) < t){
    samp <- rexp(1, lambda)
    rhos[i] <- samp
    i <- i+1
  }
  return(head(rhos, -1))
}

generate_weibull_poisson <- function(a, t, a_par) {
  poiss <- generate_poisson(lambda, t)
  len <- length(poiss)
  y_rweibull <- rweibull(len, a_par, 1)
  y_rweibull_max <- max(y_rweibull)
  K <- count(y_rweibull>y_rweibull_max-a)
  K_1 <-K[which(K$x == TRUE),'freq']
  res <- K_1
  return(res)
}

generate_weibull_poisson(a, 10, a_par) 
generate_weibull_poisson(a, 100, a_par) 
generate_weibull_poisson(a, 1000, a_par) 
generate_weibull_poisson(a, 10000, a_par) 
generate_weibull_poisson(a, 100000, a_par) 
generate_weibull_poisson(a, 1000000, a_par) 

#100

x_pweibull100 <- seq(0, 100, by = 1) 

K <- c()
for (x in 1:3001) {
  K[x] <- generate_weibull_poisson(a, 100, a_par)
} 

K
length(K)
v <- c()
for (x in x_pweibull100 ) {
  ile<- count(K<=x)
  v[x+1] <- ile[which(ile$x == TRUE),'freq']/length(K)
} 
v100<- v
v100


plot(x_pweibull100, v100, pch="_", col="blue", xlab="x", ylab="F(x)")  
legend(20, 0.2, legend=c("Dystrybuanta K "),
       col=c( "blue"), lty=1:2, cex=0.8)

#1000

x_pweibull1000 <- seq(0, 1000, by = 10) 

K <- c()
for (x in 1:3001) {
  K[x] <- generate_weibull_poisson(a, 1000, a_par)
} 

K
length(K)
v <- c()
for (x in x_pweibull1000 ) {
  ile<- count(K<=x)
  v[x/10+1] <- ile[which(ile$x == TRUE),'freq']/length(K)
} 
v1000<- v
v1000


plot(x_pweibull1000, v1000, pch="_", col="blue", xlab="x", ylab="F(x)")  
legend(200, 0.2, legend=c("Dystrybuanta K "),
       col=c( "blue"), lty=1:2, cex=0.8)

#10 000

x_pweibull10000 <- seq(0, 10000, by = 100) 

K <- c()
for (x in 1:3001) {
  K[x] <- generate_weibull_poisson(a, 10000, a_par)
} 

K
length(K)
v <- c()
for (x in x_pweibull10000 ) {
  ile<- count(K<=x)
  v[x/100+1] <- ile[which(ile$x == TRUE),'freq']/length(K)
} 
v10000<- v
v10000


plot(x_pweibull10000, v10000, pch="_", col="blue", xlab="x", ylab="F(x)")  
legend(7000, 0.2, legend=c("Dystrybuanta K "),
       col=c( "blue"), lty=1:2, cex=0.8)

#10

x_pweibull10 <- seq(0, 10, by = 0.1) 

K <- c()
for (x in 1:3001) {
  K[x] <- generate_weibull_poisson(a, 10, a_par)
} 

K
length(K)
v <- c()
for (x in x_pweibull10 ) {
  ile<- count(K<=x)
  v[x*10+1] <- ile[which(ile$x == TRUE),'freq']/length(K)
} 
v10<- v
v10
options(scipen=10000)

par(mfrow=c(1,1))
plot(x_pweibull10, v10, pch="_", col="blue", xlab="x", ylab="F(x)")  
legend(15000, 0.2, legend=c("Dystrybuanta K100000 "),
       col=c( "blue"), lty=1:2, cex=0.8)



attach(mtcars)

par(mfrow=c(2,2))

plot(x_pweibull10, v10, pch="_", col="blue", xlab="x", ylab="F(x)")  
legend(5, 0.4, legend=c("Dystrybuanta K10 "),
       col=c( "blue"), lty=1:2, cex=0.6)
plot(x_pweibull100, v100, pch="_", col="blue", xlab="x", ylab="F(x)")  
legend(50, 0.4, legend=c("Dystrybuanta K100 "),
       col=c( "blue"), lty=1:2, cex=0.6)
plot(x_pweibull1000, v1000, pch="_", col="blue", xlab="x", ylab="F(x)") 
legend(500, 0.4, legend=c("Dystrybuanta K1000 "),
       col=c( "blue"), lty=1:2, cex=0.6)
plot(x_pweibull10000, v10000, pch="_", col="blue", xlab="x", ylab="F(x)")  
legend(4800, 0.4, legend=c("Dystrybuanta K10000 "),
       col=c( "blue"), lty=1:2, cex=0.6)
