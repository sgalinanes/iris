source('machine-learning/iris/cost_function.r')

trainData <- function(X, Y, class)
{
  # Matrix dimensions
  m <- nrow(X)
  n <- ncol(X)
  k <- NROW(levels(class))
  
  # Initialize theta with zeros, adding theta_zeroes = 1.
  theta <- rbind(matrix(1, 1, k), matrix(0, n-1, k))
  
  # Loop through each class and train the parameters 
  # Using GD via the cost function w log regression
  for (i in 1:k)
  {

    theta[, i] <- costFunction(X, Y, theta[, i], levels(class)[i])
    
  }
  
  
  # Return optimized parameters
  return(theta)
  
  
}
