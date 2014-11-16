classify <- function(my_point, weights){
  my_val <- sum(my_point * weights)
  if (my_val >= 0){
    return(1)
  }else{
    return(-1)
  }
}

lin_reg <- function(my_data, my_answers, lambda=0){
  pseudo_inv <- solve(t(my_data) %*% my_data + diag(x=lambda, nrow=ncol(my_data))) %*% t(my_data)
  w <- pseudo_inv %*% my_answers
  return(w)
}

train_data <- read.table("~/Documents/workspace/caltech-ml/in.dta.txt", col.names=c("x1","x2","y"))
test_data <- read.table("~/Documents/workspace/caltech-ml/out.dta.txt", col.names=c("x1","x2","y"))

my_model = formula(~x1 + x2 + I(x1^2) + I(x2^2) + I(x1*x2) + I(abs(x1-x2)) + I(abs(x1 + x2)))

mtrain = model.matrix(my_model, train_data)
mtest = model.matrix(my_model, test_data)

w <- lin_reg(mtrain, train_in[,"y"], 10^-1)

train_data$y_hat <- apply(mtrain,1,classify, weights=w)
cat(length(which(train_data$y_hat != train_data$y)) / nrow(train_data))
test_data$y_hat <- apply(mtest,1,classify, weights=w)
cat(length(which(test_data$y_hat != test_data$y)) / nrow(test_data))

