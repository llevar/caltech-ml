problem1 <- function(){
  coins <- array(rbinom(100000*1000,10,0.5), c(100000,1000)) / 10
  random_index <- round(1000 * runif(100000))
  coin_buckets <- seq(1,100000)
  
  first_vals <- coins[coin_buckets,1] 
  #random_vals <- coins[coin_buckets,random_index]
  min_vals <- apply(coins,1, function(x) return(array(min(x))))
  
  cat(mean(min_vals))
}

problem1()