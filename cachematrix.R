#The following functions cache the inverse of matrix.

# makeCacheMatrix:
# @returns: List containing functions to set, get the value of matrix 
#           as well as its inverse
# @input: function takes matrix as an input. If not input is satisfied,
#          function creates an empyt matrix which can be modified using set
#          function

makeCacheMatrix <- function(x = matrix()) {
  invmatrix <- NULL #variable to store matrix inverse
# Set the matrix to value speficified and invmatrixto Null
  set <- function(y)
  {    x <<- y
    invmatrix <<- NULL
  }
  get <- function() x
  setinverse <- function(inv_matrix)  invmatrix <<- inv_matrix
  getinverse <- function() invmatrix
  # return a list containing getter and setter functions for matrix and inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This matrix computes the matrix inverse 
#  @input: Reference to makeCahceMatrix object
#  @returns: Matrix Inverse

cacheSolve <- function(x , ...) {
  ## The first time this function is called on an object, it will return NULL
  invmatrix <- x$getinverse()
  if(!is.null(invmatrix)){
    print("Returning Cached Results")
    return(invmatrix)
  }
  data <- x$get()
  invmatrix <- solve(data,...)
  ## Following statement sets the inverse matrix in the makeCacheMatrix object 
  x$setinverse(invmatrix)
  
  invmatrix
  
  
}



x = rbind(c(1, 2), c(2, 1))
m = makeCacheMatrix(x)
m$get()
## First Run
cacheSolve(m)
## Output:
##[,1]       [,2]
##[1,] -0.3333333  0.6666667
##[2,]  0.6666667 -0.3333333

## Second Run
cacheSolve(m)
## Output:
#[1] "Returning Cached Results"
##[,1]       [,2]
##[1,] -0.3333333  0.6666667
##[2,]  0.6666667 -0.3333333

