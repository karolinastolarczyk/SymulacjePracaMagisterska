#Ustawiamy ziarno
set.seed(17)

library('plyr')
library('EnvStats')

#Ustalamy parametr a 
a <- 10

#Ustalamy parametry a i b dla rozkładu Pareto
a_par <- 1

#deterministyczna wielkosc probki

# funkcja pomocnicza wyznaczająca K_{n:n}(a)
generate_pareto <- function(a, N, a_par) {
  y_rpareto <- rpareto(N, a_par, 1) 
  y_rpareto_max <- max(y_rpareto)
  K <- count(y_rpareto>y_rpareto_max-a)
  K_1 <-K[which(K$x == TRUE),'freq']
  res <- K_1
  return(res)
}

#generowanie dla róznych wielości próbek
generate_pareto(a, 10, a_par)
generate_pareto(a, 100, a_par)
generate_pareto(a, 1000, a_par)
generate_pareto(a, 10000, a_par)
generate_pareto(a, 100000, a_par)
generate_pareto(a, 1000000, a_par)
generate_pareto(a, 10000000, a_par)

rpareto(100, a_par, 1)

# 100

x_ppar <- seq(0, 10, by = 0.1) 

K <- c()
for (x in 1:3001) {
  K[x] <- generate_pareto(10, 100, a_par)
} 

K
length(K)
v <- c()
for (x in x_ppar ) {
  ile<- count(K<=x)
  v[x*10+1] <- ile[which(ile$x == TRUE),'freq']/length(K)
} 
v100<- v

#przesunięcie ze względu na różne koncepcje rozkładu geometrycznego
v1 <- c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)
pgeo <- c(v1, pgeom(x_ppar, prob = 1))
pgeo <- pgeo[1:101]
pgeo

plot(x_ppar, v, pch="_", col="blue", xlab="x", ylab="F(x)")  
points(x_ppar, pgeo, pch="_", col="magenta")
points(x_ppar, v, pch="_", col="blue")  
legend(6, 0.9, legend=c("Dystrybuanta 1 ","Dystrybuanta K "),
       col=c("magenta", "blue"), lty=1:2, cex=0.8)

#500

K <- c()
for (x in 1:3001) {
  K[x] <- generate_pareto(10, 500, a_par)
} 

K
length(K)
v <- c()
for (x in x_ppar ) {
  ile<- count(K<=x)
  v[x*10+1] <- ile[which(ile$x == TRUE),'freq']/length(K)
} 
v500 <- v

plot(x_ppar, v, pch="_", col="blue", xlab="x", ylab="F(x)") 
points(x_ppar, pgeo, pch="_", col="magenta")
points(x_ppar, v, pch="_", col="blue") 
legend(6, 0.98, legend=c("Dystrybuanta 1 ","Dystrybuanta K "),
       col=c("magenta", "blue"), lty=1:2, cex=0.8)

#1000

K <- c()
for (x in 1:3001) {
  K[x] <- generate_pareto(10, 1000, a_par)
} 

K
length(K)
v <- c()
for (x in x_ppar ) {
  ile<- count(K<=x)
  v[x*10+1] <- ile[which(ile$x == TRUE),'freq']/length(K)
} 
v1000 <- v


plot(x_ppar, v, pch="_", col="blue", xlab="x", ylab="F(x)") 
points(x_ppar, pgeo, pch="_", col="magenta")
points(x_ppar, v, pch="_", col="blue") 
legend(6, 0.985, legend=c("Dystrybuanta 1 ","Dystrybuanta K "),
       col=c("magenta", "blue"), lty=1:2, cex=0.8)

#zbiorczy wykres
plot(x_ppar, v100, pch="_", col="blue", xlab="x", ylab="F(x)") 
points(x_ppar, pgeo, pch="_", col="magenta")
points(x_ppar, v100, pch="_", col="blue") 
points(x_ppar, v500, pch="_", col="green") 
points(x_ppar, v1000, pch="_", col="red") 
legend(6, 0.9, legend=c("Dystrybuanta 1 ","Dystrybuanta K100 ","Dystrybuanta K500 ","Dystrybuanta K1000 "),
       col=c("magenta", "blue", 'green', 'red'), lty=1:2, cex=0.8)

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

generate_pareto_poisson <- function(a, t, a_par) {
  poiss <- generate_poisson(lambda, t)
  len <- length(poiss)
  y_rpareto <- rpareto(len, a_par, 1) 
  y_rpareto_max <- max(y_rpareto)
  K <- count(y_rpareto>y_rpareto_max-a)
  K_1 <-K[which(K$x == TRUE),'freq']
  res <- K_1
  return(res)
}

generate_pareto_poisson(a, 10, a_par) 
generate_pareto_poisson(a, 100, a_par) 
generate_pareto_poisson(a, 1000, a_par) 
generate_pareto_poisson(a, 10000, a_par) 
generate_pareto_poisson(a, 100000, a_par) 
generate_pareto_poisson(a, 1000000, a_par) 

x_ppar <- seq(0, 10, by = 0.1) 

K <- c()
for (x in 1:3001) {
  K[x] <- generate_pareto_poisson(10, 100, a_par)
} 

K
length(K)
v <- c()
for (x in x_ppar ) {
  ile<- count(K<=x)
  v[x*10+1] <- ile[which(ile$x == TRUE),'freq']/length(K)
} 
v100<- v
v100
#przesunięcie ze względu na różne koncepcje rozkładu geometrycznego
v1 <- c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)
pgeo <- c(v1, pgeom(x_ppar, prob = 1))
pgeo <- pgeo[1:101]
pgeo
x_ppar

plot(x_ppar, v, pch="_", col="blue", xlab="x", ylab="F(x)", ylim=c(0.7,1))  
points(x_ppar, pgeo, pch="_", col="magenta")
points(x_ppar, v, pch="_", col="blue")  
legend(6, 0.9, legend=c("Dystrybuanta 1 ","Dystrybuanta K "),
       col=c("magenta", "blue"), lty=1:2, cex=0.8)

#500

K <- c()
for (x in 1:3001) {
  K[x] <- generate_pareto_poisson(10, 500, a_par)
} 

K
length(K)
v <- c()
for (x in x_ppar ) {
  ile<- count(K<=x)
  v[x*10+1] <- ile[which(ile$x == TRUE),'freq']/length(K)
} 
v500 <- v

plot(x_ppar, v, pch="_", col="blue", xlab="x", ylab="F(x)") 
points(x_ppar, pgeo, pch="_", col="magenta")
points(x_ppar, v, pch="_", col="blue") 
legend(6, 0.98, legend=c("Dystrybuanta 1 ","Dystrybuanta K "),
       col=c("magenta", "blue"), lty=1:2, cex=0.8)

#1000

K <- c()
for (x in 1:3001) {
  K[x] <- generate_pareto_poisson(10, 1000, a_par)
} 

K
length(K)
v <- c()
for (x in x_ppar ) {
  ile<- count(K<=x)
  v[x*10+1] <- ile[which(ile$x == TRUE),'freq']/length(K)
} 
v1000 <- v


plot(x_ppar, v, pch="_", col="blue", xlab="x", ylab="F(x)") 
points(x_ppar, pgeo, pch="_", col="magenta")
points(x_ppar, v, pch="_", col="blue") 
legend(6, 0.985, legend=c("Dystrybuanta 1 ","Dystrybuanta K "),
       col=c("magenta", "blue"), lty=1:2, cex=0.8)

#zbiorczy wykres
plot(x_ppar, v100, pch="_", col="blue", xlab="x", ylab="F(x)", ylim=c(0.7,1)) 
points(x_ppar, pgeo, pch="_", col="magenta")
points(x_ppar, v500, pch="_", col="green") 
points(x_ppar, v1000, pch="_", col="red") 
legend(6, 0.85, legend=c("Dystrybuanta 1 ","Dystrybuanta K100 ","Dystrybuanta K500 ","Dystrybuanta K1000 "),
       col=c("magenta", "blue", 'green', 'red'), lty=1:2, cex=0.8)

