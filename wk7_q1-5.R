classify <- function(my_point, weights){
  my_val <- sum(my_point * weights)
  if (my_val >= 0){
    return(1)
  }else{
    return(-1)
  }
}

get_classification_error <- function(my_data){
  (length(which(my_data$y_hat != my_data$y)) / nrow(my_data))
}

lin_reg <- function(my_data, my_answers, lambda=0){
  pseudo_inv <- solve(t(my_data) %*% my_data + diag(x=lambda, nrow=ncol(my_data))) %*% t(my_data)
  w <- pseudo_inv %*% my_answers
  return(w)
}


validation <- function(){
  train_data <- read.table("~/Documents/workspace/caltech-ml/in.dta.txt", col.names=c("x1","x2","y"))
  validation_data <- train_data[26:dim(train_data)[1],]
  train_data <- train_data[1:25,]
  test_data <- read.table("~/Documents/workspace/caltech-ml/out.dta.txt", col.names=c("x1","x2","y"))
  
  my_model <- formula(~x1 + x2 + I(x1^2) + I(x2^2) + I(x1*x2) + I(abs(x1-x2)) + I(abs(x1 + x2)))
  
  mtrain <- model.matrix(my_model, train_data)
  mvalidation <- model.matrix(my_model, validation_data)
  mtest <- model.matrix(my_model, test_data)
  
  for(i in 4:8){
    w <- lin_reg(mtrain[, 1:i], train_data[,"y"])
    validation_data$y_hat <- apply(mvalidation[, 1:i],1,classify, weights=w)
    cat("Validation set performance on i=", i-1, ":\n",get_classification_error(validation_data), "\n")
    
    test_data$y_hat <- apply(mtest[, 1:i],1,classify, weights=w)
    cat("Test set performance:\n",get_classification_error(test_data), "\n")
  }  
    
  w <- lin_reg(mtrain, train_data[,"y"])
  
  train_data$y_hat <- apply(mtrain,1,classify, weights=w)
  cat(get_classification_error(train_data))
  test_data$y_hat <- apply(mtest,1,classify, weights=w)
  cat(get_classification_error(test_data))
}

validation()
