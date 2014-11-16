my_err <- function(x){
  return ((x[1]*exp(x[2]) - 2 * x[2] * exp(-1 * x[1]))^2)
}

grad_err <- function(x){
  my_result <- c(2 * (x[1] * exp(x[2]) - 2 * x[2] * exp(-1 * x[1])) * (exp(x[2]) + 2 * x[2] * exp(-1 * x[1])), 2 * (x[1] * exp(x[2]) - 2 * x[2] * exp(-1 * x[1])) * (x[1] * exp(x[2]) - 2 * exp(-1 * x[1])))
  cat(x, " ", my_result,"\n")
  return(my_result)
}

gradient_descent <- function(start, learning_rate){
  uv = start  
  cur_err <- my_err(start)  
  i <- 1
  
  repeat{
    if(cur_err < 10^-14){ break }
    delta_e <- grad_err(uv)
    uv <- uv - learning_rate * delta_e
    cur_err <- my_err(uv)
    cat(uv, " ", delta_e, " ", cur_err," ", i, "\n")
    i <- i + 1
  }
  uv
}

final_uv <- gradient_descent(c(1,1), 0.1)
final_uv

dist <- function(x,y){
  return(sqrt(sum((x-y)^2)))
}

dist(final_uv, c(1.000,1.000))
dist(final_uv, c(0.713, 0.045))
dist(final_uv, c(0.016, 0.112))
dist(final_uv, c(-0.083, 0.029))
dist(final_uv, c(0.045, 0.024))

coordinate_descent <- function(start, learning_rate){
  uv = start  
  cur_err <- my_err(start)  
  i <- 1
  
  repeat{
    if(i > 15){ break }
    delta_e <- grad_err(uv)
    uv <- uv - learning_rate * (c(1,0) * delta_e)
    delta_e <- grad_err(uv)
    uv <- uv - learning_rate * (c(0,1) * delta_e)
    cur_err <- my_err(uv)
    cat(uv, " ", delta_e, " ", cur_err," ", i, "\n")
    i <- i + 1
  }
  uv
}

final_uv <- coordinate_descent(c(1,1), 0.1)