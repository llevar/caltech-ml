library(ggplot2)

line_y <- function(x){
  return (my_slope * x + my_intercept)
}

separate <- function(x){
  line_val <- line_y(x[1])
  
  if (x[2] > line_val){
    return (1)
  } else {
    return (-1)
  }
}

classify <- function(my_point){
  my_val <- sum(c(my_point,1) * w)
  if (my_val >= 0){
    return(1)
  }else{
    return(-1)
  }
}

update_weights <- function(update_index){
  return (w + my_answers[update_index] * c(my_data[update_index,],1))
}

d <- 2
N <- 100

total_iter <- 0

for (i in 1:1000){
  plot.new()
  print(i)
  pt_x <- 2 * runif(2) - 1
  pt_y <- 2 * runif(2) - 1
  
  my_slope <- (pt_y[2] - pt_y[1]) / (pt_x[2] - pt_x[1])
  my_intercept <- pt_y[1] - my_slope * pt_x[1]
  
  my_data <- matrix(2 * runif(N * 2) - 1, ncol=2)
  my_answers <- apply(my_data,1,separate)
  
  #plot(my_data, col=ifelse(my_answers == 1, 'blue', ifelse(my_answers == -1, 'green', 'red')))
  #points(pt_x,pt_y,pch="x")
  #abline(my_intercept, my_slope,col="green")
 
  w <- c(0,0,0)
  iter_ctr <- 0
  
  while(TRUE){
    l_slope = 0
    l_intercept = 0
    
    if(w[2] != 0){
      l_slope = -1 * w[1] / w[2]
      l_intercept = -1 * w[3] / w[2]
    }
    #print(w)
    my_classes <- apply(my_data,1,classify)
    
    #plot(my_data, pch=ifelse(my_classes != my_answers, 'x', 'o'),col=ifelse(my_answers == 1, 'blue', ifelse(my_answers == -1, 'green', 'red')))
    #abline(l_intercept,l_slope, col="red")
    #abline(my_intercept, my_slope,col="green")
    #Sys.sleep(0.5)
    if(all(my_answers == my_classes)){
      total_iter <- total_iter + iter_ctr
      break
    }
    
    iter_ctr <- iter_ctr + 1
    mismatches <- which(my_answers != my_classes)
    
    if(length(mismatches) == 1){
      learning_index = mismatches
    }else{
      learning_index = sample(mismatches,1)
    }
    
    my_data[learning_index,]
    
    
    w<- update_weights(learning_index)
    
  }
}

print(total_iter / 1000)