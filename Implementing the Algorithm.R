sigmoid <- function(z){
  1/(1 + exp(-z))
}

normalize <- function(X){
  (X - mean(X))/ sd(X)
}

negative_log_likelihood <- function(X, y, w){
  f = sigmoid(X %*% w)
  -sum((y * log(f)) + ((1 - y) * log(1 - f)))
}


gradient_descent <- function(X, y, w, learning_rate, epochs, stop_rule){
  count = 0
  for(i in 1:epochs){
    loss <- negative_log_likelihood(X, y, w)
    w = w - learning_rate * t(X) %*% (sigmoid(X %*% w)-y) # w = w - lr * gradient
    updated_loss <- negative_log_likelihood(X, y, w)
    
    # Stopping Rule
    if(stop_rule){
      if(loss - updated_loss < 0.01){
        count = count + 1
        if(count == 10){
          return(w)
        }
      }
    }
  }
  
  return(w)
}

fit <- function(X, y, w = rep(0, ncol(X)), learning_rate = 0.01, epochs = 1000, stop_rule = F){
  w <- gradient_descent(X, y, w, learning_rate, epochs,stop_rule)
  return(list(best_params = w))
}

predict <- function(X, w){
  preds <- c(rep(0), nrow(X))
  for (i in 1:nrow(X)){
    preds[i] <- sigmoid(X[i,] %*% w) # Stores the probability of the label being equal to 1
  }
  return(preds)
}

