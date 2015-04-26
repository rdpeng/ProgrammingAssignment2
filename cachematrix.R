#Running Example
#a <- makeCacheMatrix(matrix(c(1,2,3,4), nrow=2, ncol=2))
#a$GetMatrix()
#a$GetInverseMatrix()
#cacheSolve(a)

## Write a short comment describing this function
#makeCacheMatrix is a function that returns a list that contains the following functions
#SetMatrix: Set the values of the matrix
#GetMatrix: Get the values of the matrix
#SetInverseMatrix: Set the Inverse values of a matrix
#GetInverseMatrix: Get the Inverse Values of the Matrix

makeCacheMatrix <- function(x = matrix()) {
  
  #Matrix instantiate to NULL
  m <- NULL
  
  #Set 
  SetMatrix <- function(y)
  {
    x <<- y
    m <<- NULL
    
  }
  #return the matrix
  GetMatrix <- function()
  {
    x
  }
  
  SetInverseMatrix <- function(InverseMatrix)
  {
    m <<- InverseMatrix
  }
  
  GetInverseMatrix <- function()
  {
    m
  }
  
  list( GetMatrix = GetMatrix, SetInverseMatrix = SetInverseMatrix, GetInverseMatrix = GetInverseMatrix)

}


## Write a short comment describing this function
#cacheSolve is a functin that either calculates the inverse of a matrix or returns the cached one
#If the inverse has been calculated before, it returns the cached one else it calcualtes the inverse
#save it in the cache and then returns it

cacheSolve <- function(x, ...) {
  ## Set the matrix
  m <- x$GetInverseMatrix()
  
  #check if the inverse already exists
  if(!is.null(m))
  {
    message("getting cached data")
    return(m)
  }
  
  #The invrese is not cached so do the calculations
  data <- x$GetMatrix()
  m <- solve(data)  
  x$SetInverseMatrix(m)    
  m
  
}
