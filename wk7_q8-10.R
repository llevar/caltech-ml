library(ggplot2)

line_y <- function(x){
  return (my_slope * x + my_intercept)
}

make_random_line <- function(){
  pt_x <- 2 * runif(2) - 1
  pt_y <- 2 * runif(2) - 1
  
  my_slope <- (pt_y[2] - pt_y[1]) / (pt_x[2] - pt_x[1])
  my_intercept <- pt_y[1] - my_slope * pt_x[1]
  return(list("slope" = my_slope, "intercept" = my_intercept))
}

separate <- function(x, my_line){
  line_val <- my_line$slope * x[1] + my_line$intercept
  
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

update_weights <- function(miss,weights){
  new_w <- weights + miss[4] * miss[1:3]
  return (new_w)
}

total_iter <- 0
total_e_out_pla <- 0
total_e_out_svm <- 0
svm_wins <- 0
svm_count <- 0

init_param = list("d" = 2, 
                  "num_training" = 100, 
                  "num_sample" = 10000, 
                  "num_replicates" = 1000,
                  "test_on_sample" = TRUE)

do_pla <- function(){
  for (i in 1:init_param$num_replicates){
    print(i)
    
    my_line <- make_random_line()
    
    my_training_data <- matrix(2 * runif(init_param$num_training * init_param$d) - 1, ncol=init_param$d)
    my_training_data <- cbind(1,my_training_data)
    my_training_data <- cbind(my_training_data, apply(my_training_data[,2:3], 1, separate, my_line = my_line))
    
    my_training_data <- cbind(my_training_data, 0)
    
    w <- c(0,0,0)
    iter_ctr <- 0
    
    while(TRUE){
      #plot.new()
      l_slope = 0
      l_intercept = 0
      
      if(w[3] != 0){
        l_slope = -1 * w[1] / w[3]
        l_intercept = -1 * w[2] / w[3]
      }
      my_training_data[,5] <- apply(my_training_data[,1:I(init_param$d+1)],1,classify, weights=w)
      #plot(my_training_data[,2:3], pch=ifelse(my_training_data[,4] != my_training_data[,5], 'x', 'o'),col=ifelse(my_training_data[,4] == 1, 'blue', 'green'))
      #abline(l_intercept,l_slope, col="red")
      #abline(my_line$intercept, my_line$slope,col="green")
      #Sys.sleep(0.5)
      if(all(my_training_data[,4] == my_training_data[,5])){
        total_iter <- total_iter + iter_ctr
        break
      }
      
      iter_ctr <- iter_ctr + 1
      mismatches <- which(my_training_data[,4] != my_training_data[,5])
      
      if(length(mismatches) == 1){
        learning_index = mismatches
      }else{
        learning_index = sample(mismatches,1)
      }
      
      my_training_data[learning_index,]
      
      w <- update_weights(my_training_data[learning_index,], w)
    }
    
    my_sample_data <- matrix(2 * runif(init_param$num_sample * init_param$d) - 1, ncol=init_param$d)
    my_sample_data <- cbind(1,my_sample_data)
    my_sample_data <- cbind(my_sample_data, apply(my_sample_data[,2:3], 1, separate, my_line = my_line))
    my_sample_data <- cbind(my_sample_data, apply(my_sample_data[,1:I(init_param$d+1)],1,classify, weights=w))
    num_errors_pla <- sum(my_sample_data[,4] != my_sample_data[,5])
    total_e_out_pla <- total_e_out_pla + num_errors_pla
    
    Vmat <- (my_training_data[,4] * my_training_data[,2:3]) %*% t(my_training_data[,4] * my_training_data[,2:3])
    dvec <- rep(-1, init_param$num_training)
    Amat <- t(my_training_data[,4])
    uvec <- rep(10000, init_param$num_training)
    solution <- LowRankQP(Vmat=Vmat,dvec=dvec,Amat=Amat,bvec=0,uvec=uvec,method="LU")
    a <- c(zapsmall(solution$alpha, digits=6))
    #cat(a,"\n")
    svm_count <- svm_count + sum(a > 0)
    new_w <- apply(a * my_training_data[,4] * my_training_data[,2:3],2,sum)
    
    first_sv <- which(a > 0)[1]
    b <- (1 - my_training_data[first_sv,4] * (sum(new_w * my_training_data[first_sv,2:3]))) / my_training_data[first_sv,4]
    svm_w <- (c(b,new_w))
    my_sample_data <- cbind(my_sample_data, apply(my_sample_data[,1:I(init_param$d+1)],1,classify, weights=svm_w))
    num_errors_svm <- sum(my_sample_data[,4] != my_sample_data[,6])
    if(num_errors_svm < num_errors_pla){
      svm_wins <- svm_wins + 1
    }
    total_e_out_svm <- total_e_out_svm + num_errors_svm
  }
  
  cat("Avergae number of epochs to conversion: ", total_iter / init_param$num_replicates,"\n")
  cat("Average out of sample error for PLA:", total_e_out_pla / (init_param$num_sample * init_param$num_replicates),"\n")
  cat("Average out of sample error for SVM:", total_e_out_svm / (init_param$num_sample * init_param$num_replicates),"\n")
  cat("Rate of SVM wins over PLA:", svm_wins / init_param$num_replicates)
  cat("Average support vector count: ", svm_count / init_param$num_replicates)
}

do_pla()