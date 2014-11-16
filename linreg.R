library(ggplot2)
library(MASS)

make_random_line <- function(){
  pt_x <- 2 * runif(2) - 1
  pt_y <- 2 * runif(2) - 1
  
  my_slope <- (pt_y[2] - pt_y[1]) / (pt_x[2] - pt_x[1])
  my_intercept <- pt_y[1] - my_slope * pt_x[1]
  return(list("slope" = my_slope, "intercept" = my_intercept))
}

line_from_weights <- function(w){
  l_slope = 0
  l_intercept = 0
  
  if(w[2] != 0){
    l_slope = -1 * w[1] / w[2]
    l_intercept = -1 * w[3] / w[2]
  }
  
  return(list("slope" = l_slope, "intercept" = l_intercept))
  
}

separate <- function(x, my_line){
  line_val <- my_line$slop * x[1] + my_line$intercept
  
  if (x[2] >= line_val){
    return (1)
  } else {
    return (-1)
  }
}

classify <- function(my_point, weights){
  my_val <- sum(my_point * weights)
  if (my_val >= 0){
    return(1)
  }else{
    return(-1)
  }
}

lin_reg <- function(my_data, my_answers){
  pseudo_inv <- ginv(t(my_data) %*% my_data) %*% t(my_data)
  w <- pseudo_inv %*% my_answers
  return(w)
}

pla_update_weights <- function(my_data, my_answers, update_index, w_param){
  cat(my_data[update_index,])
  return (w_param + my_answers[update_index] * my_data[update_index,])
}

apply_pla <- function(my_data, true_answers, w){
  iter_ctr <- 0
  
  while(TRUE){
    learned_line <- line_from_weights(w)
    
    my_classes <- apply(my_data,1,classify)
    
    if(all(true_answers == my_classes)){
      break
    }
    
    iter_ctr <- iter_ctr + 1
    mismatches <- which(true_answers != my_classes)
    cat(mismatches)
    if(length(mismatches) == 1){
      learning_index = mismatches
    }else{
      learning_index = sample(mismatches,1)
    }
    
    w <- pla_update_weights(my_data, true_answers, learning_index, w)
  }
  
  return(iter_ctr)
}

w2_q5 <- function(){
  init_param = list("d" = 2, 
                    "num_training" = 10, 
                    "num_sample" = 1000, 
                    "num_replicates" = 1,
                    "test_on_sample" = FALSE,
                    "apply_pla" = FALSE)
  total_iter <- 0
  
  e_in = 0
  e_out = 0
  
  for (i in 1:init_param$num_replicates){
    
    my_line <- make_random_line()
    my_training_data <- matrix(c(2 * runif(init_param$num_training * 2) - 1,rep(1,init_param$num_training)), ncol=3)
    #my_line <- saved_line
    #my_training_data <- saved_data
    true_answers <- apply(my_training_data,1,separate, my_line = my_line)
    
    w <- lin_reg(my_training_data, true_answers)
    my_classes <- apply(my_training_data,1,classify)
    e_in = e_in + sum(my_classes != true_answers) / init_param$num_training
    
    if(init_param$test_on_sample){
      my_sample_data <- matrix(c(2 * runif(init_param$num_sample * 2) - 1,rep(1,init_param$num_sample)), ncol=3)
      my_out_answers <- apply(my_sample_data,1,separate, my_line = my_line)
      my_out_classes <- apply(my_sample_data, 1, classify)
      e_out = e_out + sum(my_out_classes != my_out_answers) / init_param$num_sample
    }
    
    learned_line <- line_from_weights(w)
    
    plot.new()
    plot(my_training_data, pch=ifelse(my_classes != true_answers, 'x', 'o'),col=ifelse(true_answers == 1, 'blue', ifelse(true_answers == -1, 'green', 'red')),xlim=c(-2,2),ylim=c(-2,2))
    abline(learned_line$intercept,learned_line$slope, col="red")
    abline(my_line$intercept, my_line$slope,col="green")
    Sys.sleep(2)
    
    if(init_param$apply_pla){
      total_iter <- total_iter + apply_pla(my_training_data, true_answers, w)
      
      learned_line <- line_from_weights(w)
      
      plot.new()
      plot(my_training_data, pch=ifelse(my_classes != true_answers, 'x', 'o'),col=ifelse(true_answers == 1, 'blue', ifelse(true_answers == -1, 'green', 'red')),xlim=c(-2,2),ylim=c(-2,2))
      abline(learned_line$intercept,learned_line$slope, col="red")
      abline(my_line$intercept, my_line$slope,col="green")
      Sys.sleep(2)
    }
  }
}

separate2 <- function(x){
  val <- x[1]^2 + x[2]^2 - 0.6
  
  err <- rbinom(1,1,0.1)
  
  if (err == 1){
    val <= val * -1
  }
  
  if (val >= 0){
    return(1)
  }else{
    return(-1)
  }
}

classify2 <- function(my_point){
  my_val <- my_point[1] * my_point[1] * w[1] + my_point[2] * my_point[2] * w[2] - 0.6 + w[3]
  if (my_val >= 0){
    return(1)
  }else{
    return(-1)
  }
}

w2_q8 <- function(){
  init_param = list("d" = 2, 
                    "num_training" = 1000, 
                    "num_sample" = 1000, 
                    "num_replicates" = 1000,
                    "test_on_sample" = FALSE,
                    "apply_pla" = TRUE)
  total_iter <- 0
  
  e_in = 0
  e_out = 0
  
  for (i in 1:init_param$num_replicates){
    my_training_data <- matrix(c(2 * runif(init_param$num_training * 2) - 1,rep(1,init_param$num_training)), ncol=3)
    true_answers <- apply(my_training_data,1,separate2)
    
    
    w <- lin_reg(my_training_data, true_answers)
    my_classes <- apply(my_training_data,1,classify)
    
    e_in = e_in + sum(my_classes != true_answers) / init_param$num_training
    
    
    
  }
  
  cat(e_in / init_param$num_replicates)
}

#w2_q8()

w2_q9 <- function(){
  init_param = list("d" = 2, 
                    "num_training" = 1000, 
                    "num_sample" = 1000, 
                    "num_replicates" = 1000,
                    "test_on_sample" = TRUE,
                    "apply_pla" = TRUE)
  total_iter <- 0
  
  e_in = 0
  e_out = 0
  
  sum_w = c(0,0,0,0,0,0)
  
  for (i in 1:init_param$num_replicates){
    my_training_data <- matrix(c(2 * runif(init_param$num_training * 2) - 1,rep(1,init_param$num_training)), ncol=3)
    true_answers <- apply(my_training_data,1,separate2)
    
    transformed_data <- cbind(my_training_data, my_training_data[,1] * my_training_data[,2], my_training_data[,1]^2, my_training_data[,2]^2)
    
    w <- lin_reg(transformed_data, true_answers)
    sum_w <- sum_w + w
    my_classes <- apply(transformed_data,1,classify, weights = w)
    
    e_in = e_in + sum(my_classes != true_answers) / init_param$num_training
    
    if(init_param$test_on_sample){
      my_sample_data <- matrix(c(2 * runif(init_param$num_sample * 2) - 1,rep(1,init_param$num_sample)), ncol=3)
      sample_transformed_data <- cbind(my_sample_data, my_sample_data[,1] * my_sample_data[,2], my_sample_data[,1]^2, my_sample_data[,2]^2)
      
      my_out_answers <- apply(my_sample_data,1,separate2)
      my_out_classes <- apply(sample_transformed_data, 1, classify, weights = w)
      e_out = e_out + sum(my_out_classes != my_out_answers) / init_param$num_sample
    }
    
  }
  
  cat(e_in / init_param$num_replicates,"\n")
  cat(e_out / init_param$num_replicates,"\n")
  cat(sum_w / init_param$num_replicates,"\n")
}

w2_q9()