##  INVERSE MATRIX FUNCTION

## The first function, makeCacheMatrix creates the MATRIX which sets the values for thge matrix, 
## then it tests for the value to be in the enviroment cache
## 

makeCacheMatrix <- function(x = matrix()) {
  reverse <- NULL
  set <- function (y) {
  x <<- y
  reverse <<- NULL
  }
  get <- function() x
  setreverse <- function(inverse) reverse <<- inverse
  getreverse <- function() reverse
  list (set = set, get = get, setreverse = setreverse, getreverse = getreverse)
  
}

## The second calculates the inverse of X whcih is the matrix

cacheSolve <- function(x, ...) {
        reverse <- x$getreverse()
        if(!is.null(reverse)) {
                message("Getting cached data")
                return(reverse)
        }
        data <- x$get()
        reverse <- solve(data,...)
        x$setreverse(reverse)
        reverse
}
