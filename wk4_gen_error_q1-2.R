dvc <- 50
delta <- 0.05

mh <- function(x){
  if(x > dvc){
    return(x^dvc)
  }else{
    return(2^x)
  }
}


N <- 10000
vc <- function(N){
  return(sqrt(8 / N * log(4*mh(2*N) / delta)))
}

rademacher <- function(N){
  return(sqrt(2 * log(2*N*mh(N) / N)) + sqrt((2 / N) * log(1 / delta)) + 1 / N)
}

parrondo_slv <- function(x){
  y <- sqrt(1 / N * (2 * x + log(6 * mh(2 * N) / delta))) - x
  return(y)
}

devroye_slv <- function(x){
  y <- sqrt((1 / (2 * N)) * (4 * x * (1 + x) + log(4) + 2 * dvc * log(N) - log(delta))) - x
  return(y)
}


parrondo <- function(N){
  epsilon_par <- nleqslv(c(0.05), parrondo_slv, control=list(btol=0.01))
  return(sqrt(1 / N * (2 * epsilon_par$x + log(6 * mh(2 * N) / delta))))
}

devroye <- function(N){
  epsilon_devroye <- nleqslv(c(0.05), devroye_slv, control=list(btol=0.01))
  return(sqrt(1 / (2 * N) * (4 * epsilon_devroye$x * (1 + epsilon_devroye$x) + log(4) + 2 * dvc * log(N) - log(delta))))
}

vc(N)
rademacher(N)
parrondo(N)
devroye(N)

N <- 5

devroye_slv2 <- function(x){
  y <- sqrt((1 / (2 * N)) * (4 * x * (1 + x) + log(4) + 2 * N * log(2) - log(delta))) - x
  return(y)
}

devroye2 <- function(N){
  epsilon_devroye <- nleqslv(c(0.05), devroye_slv2, control=list(btol=0.01))
  return(sqrt(1 / (2 * N) * (4 * epsilon_devroye$x * (1 + epsilon_devroye$x) + log(4) + 2 * N * log(2) - log(delta))))
}

vc(N)
rademacher(N)
parrondo(N)
devroye2(N)

