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
  line_val <- my_line$slope * x[1] + my_line$intercept
  
  if (x[2] >= line_val){
    return (1)
  } else {
    return (-1)
  }
}

f <- function(x, my_line){
  line_val <- my_line$slope * x[2] + my_line$intercept
  
  if (x[3] >= line_val){
    return (1)
  } else {
    return (-1)
  }
}

classify <- function(my_point, weights){
  my_val <- sum(my_point[1:3] * weights)
  
  if (my_val >= 0){
    return(1)
  }else{
    return(-1)
  }
}

cross_entropy_grad <- function(x,y,weights){
  return (-1 * (x * y) / (1 + exp(y * sum(weights * x))))
}

cross_entropy <- function(x,y,weights){
  return (log(1 + exp(-1 * y * sum(weights * x))))
}

init_param = list("d" = 2, 
                  "num_training" = 100, 
                  "num_sample" = 100, 
                  "num_replicates" = 1,
                  "learning_rate" = 0.01,
                  "stopping_threshold" = 0.01,
                  "test_on_sample" = TRUE)
total_iter <- 0

e_in = 0
e_out = 0
#last_w <- w
avg_epochs <- 0

for (i in 1:init_param$num_replicates){
  avg_err <- 0
  my_line <- make_random_line()
  
  my_training_data <- matrix(c(2 * runif(init_param$num_training * init_param$d) - 1,rep(1,init_param$num_training)), ncol=(init_param$d + 1))
  
  true_answers <- apply(my_training_data,1,separate, my_line = my_line)
  
  w <- rep(0, times=init_param$d + 1)
  last_w <- w
  ctr <- 0
  repeat{
    ctr <- ctr + 1
    my_ordering <- sample(init_param$num_training)
    for(j in 1:init_param$num_training){
      ind <- my_ordering[j]
      w <- w - init_param$learning_rate * cross_entropy_grad(my_training_data[ind,],true_answers[ind],w)
    }
    cat(sqrt(sum((w - last_w)^2)),"\n")
    if(sqrt(sum((w - last_w)^2)) < init_param$stopping_threshold){ 
      avg_epochs <- avg_epochs + ctr
      cat(ctr, "\n")
      break 
    }
    last_w <- w
  }
  
  learned_line <- line_from_weights(w)
  my_classes <- apply(my_training_data,1,classify,weights=w)
  
  if(init_param$test_on_sample){
    my_test_data <- matrix(c(2 * runif(init_param$num_sample * init_param$d) - 1,rep(1,init_param$num_sample)), ncol=(init_param$d + 1))
    test_true_answers <- apply(my_test_data,1,separate, my_line = my_line)
    my_test_classes <- apply(my_test_data,1,classify,weights=w)
    
    cur_err <- 0
    for (i in 1:init_param$num_sample){
      cur_err <- cur_err + cross_entropy(my_test_data[i,],test_true_answers[i],w)
    }
    avg_err <- avg_err + cur_err
    cat(cur_err / init_param$num_sample, "\n")
  }
  
  
}

cat(avg_err / init_param$num_replicates, "\n")
cat(avg_epochs / init_param$num_replicates, "\n")
