source('R/iris/sigmoid.r')

costFunction <- function(X, Y, theta, cur_class)
{
  m <- nrow(X)
  n <- ncol(X)

  # Make logical matrix out of Y, such that cur_class = 1 and others = 0.
  Y <- (Y == cur_class)
  
  # Calculate theta' * x
  A <- X%*%theta
  
  # Apply the sigmoid function to each value in vector A.
  # H is now a vector containing the hypothesis functions for every example.
  H <- sigmoid(A)
  
  # Calculate the cost function.
  J <- (-1/m) * ( t(Y)%*%log(H) + ( t(1.0 - Y) %*% log(1.0 - H) ) )
  
  # Calculate the gradient
  grad <- (1/m) * ( t(X)%*%(H-Y) )
  
  # Apply batch Gradient Descent
  ITER <- 500000
  alpha <- 0.15
  TOL <- 10^-10
  #plotter <- matrix(NA, ITER, 1)
  for(i in 1:ITER)
  {
    ch = alpha*grad
    print(mean(ch))
    if(abs(mean(ch)) < TOL)
    {
      break
    }
    theta <- theta - ch
    A <- X%*%theta
    H <- sigmoid(A)
    J <- (-1/m) * ( t(Y)%*%log(H) + ( t(1.0 - Y) %*% log(1.0 - H) ) )
    grad <- (1/m) * ( t(X)%*%(H-Y) )
    #plotter[i] <- J
  }
  
  message(sprintf("Iterated for %i", i))
  
  #plot(plotter, type="o", col="blue")
  
  return(theta)
  
}


