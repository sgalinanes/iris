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
