# Iris classification problem by R. A. Fisher.
# Given a data set of 150 flowers with the following attributes:
# Attribute Information:

# 1. sepal length in cm
# 2. sepal width in cm
# 3. petal length in cm
# 4. petal width in cm
# 5. class:
#   -- Iris Setosa
# -- Iris Versicolour
# -- Iris Virginica

# Sources
source('R/iris/logistic_regression.r')
source('R/iris/sigmoid.r')
# Load libraries
# library(caret)


# Determine which class it belongs to based on the labeled data.

# Load the data.
iris <- read.csv("https://archive.ics.uci.edu/ml/machine-learning-databases/iris/iris.data", header = FALSE)

# Add names to columns.
colnames(iris) <- c("s_len", "s_wid", "p_len", "p_wid", "class")

# Plot correlation of variables with class colouring.
pairs(class~., data=iris, col = iris$class)

# Plot density of variables by class
# x <- iris[, 1:4]
# y <- iris[, 5]
# scales <- list(x = list(relation="free"), y = list(relation = "free"))
# featurePlot(x=x, y=y, plot="density", scales=scales)

# Save data in matrices.
X <- data.matrix(iris[, 1:4])
ones <- matrix(1, nrow(iris), 1)
X <- cbind(ones, X)
Y <- data.matrix(iris[, 5])

# Call the trainer
theta <- trainData(X, Y, iris$class)

# Call the predictor
test <- rbind( data.matrix(X[1:10, ]), data.matrix(X[60:70, ]), data.matrix(X[120:130, ]) )
results <- rbind( data.matrix(Y[1:10, ]), data.matrix(Y[60:70]), data.matrix(Y[120:130, ]) )
A <- test%*%theta
H <- sigmoid(A)
print(H)

for(i in 1:nrow(H))
{
  index <- which.max(H[i, ])
  
  if(index == 1)
  {
    str <- "Iris-setosa"

  }
  else if(index == 2)
  {
    str <- "Iris-versicolor"
  }   
  else 
  {
    str <- "Iris-virginica"
  }
  
  message(sprintf("Line %i, class is %s", i, str))
  if(str == results[i])
  {
    print("Correct")
  }
  else
  {
    print("Incorrect")
  }
}


