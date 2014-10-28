require(ggplot2)
N <- 10000
x <- matrix(2*runif(2 * N) - 1, ncol=2)

my_sin <- function(x){
  return(sin(pi*x))
}

y <- my_sin(x)

plot(my_sin,-1,1)

my_h <- function(a,x){
  return(a*x)
}

my_err <- function(a, d){
  apply((my_h(a,d) - my_sin(d))^2,1, sum)
}

a_hat = (y[,1]*x[,1] + y[,2]*x[,2]) / (x[,1]^2 + x[,2]^2)
errors <- my_err(a_hat,x)
errors2 <- my_err(a_hat + 0.001,x)
errors3 <- my_err(a_hat - 0.001, x)
e_a_hat <- mean(a_hat)
abline(0,e_a_hat)

e_a_hat
mean(errors)

bias <- mean((my_h(e_a_hat,x) - y) ^ 2)
bias

variance <- mean(apply((my_h(a_hat,x) - my_h(e_a_hat,x))^2,1,mean))
variance

my_h2 <- function(a,d){
  return(a*x^2)
}

my_err2 <- function(a, d){
  apply((my_h2(a,d) - my_sin(d))^2,1, sum)
}

a_hat2 = (y[,1] + y[,2]) / (x[,1]^2 + x[,2]^2)

errors_2 <- my_err(a_hat2,x)
errors2_2 <- my_err(a_hat2 + 0.001,x)
errors3_2 <- my_err(a_hat2 - 0.001, x)
e_a_hat2 <- mean(a_hat2)
#abline(0,e_a_hat)

e_a_hat2
mean(errors_2)

bias <- mean((my_h2(e_a_hat2,x) - y) ^ 2)
bias

variance <- mean(apply((my_h2(a_hat2,x) - my_h2(e_a_hat2,x))^2,1,mean))
variance

