## A pair of functions that cache the inverse of a matrix rather than compute it repeatedly. 

## function below creates the matrix object that cache the inverse

makeCacheMatrix <- function(x = matrix()) {}
  inverse <- NULL
  set <- function (y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(solveMatrix) inverse <<- solveMatrix
  getinverse <- function() inverse
  list(set = set, get = get, 
       setinverse = setinverse, 
       getinverse = getinverse)
}


## function below computes the inverse of the matrix that resulted from above 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data)
  x$setinverse(inverse)
  inverse
}
