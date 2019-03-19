costFunction <- function(X, Y, theta, cur_class)
{
  m <- nrow(X)
  n <- ncol(X)

  # Make logical matrix out of Y, such that cur_class = 1 and others = 0.
  Y <- (Y == cur_class)
  
  # Calculate theta' * x
  A <- X%*%theta
  print(A)
  # Apply the sigmoid function to each value in vector A.
  # H is now a vector containing the hypothesis functions for every example.
  H <- sigmoid(A)
  print(H)
  
  # Calculate the cost function.
  print(log(H))
  J <- (-1/m) * ( t(Y)%*%log(H) + ( t(1.0 - Y) %*% log(1.0 - H) ) )
  
  # Calculate the gradient
  grad <- (1/m) * ( t(X)%*%(H-Y) )
  
  # Apply batch Gradient Descent
  ITER <- 20000
  alpha <- 0.15
  plotter <- matrix(NA, ITER, 1)
  for(i in 1:ITER)
  {
    
    theta <- theta - alpha*grad
    A <- X%*%theta
    H <- sigmoid(A)
    J <- (-1/m) * ( t(Y)%*%log(H) + ( t(1.0 - Y) %*% log(1.0 - H) ) )
    grad <- (1/m) * ( t(X)%*%(H-Y) )
    plotter[i] <- J
  }
  
  plot(plotter, type="o", col="blue")
  
  print(J)
  
  return(theta)
  
}

sigmoid <- function(A)
{
  m <- NROW(A)
  n <- NCOL(A)
  
  H <- matrix(0, m, n)
  
  for(i in 1:m)
  {
    for(j in 1:n)
    {
      
      H[i, j] = 1.0 / (1.0 + exp(-A[i, j]))
      
    }
  }
  
  
  return(H)
}

