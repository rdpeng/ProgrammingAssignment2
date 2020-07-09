## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  j <- NULL                       ## initialize j as NULL
  set <- function(y){             ##define the set function to assign new
    x <<- y                         ##value of matrix in parent environment
    inv <<- NULL                    ## if there is new matrix, reset j to NULL
  }
  get <- function() x
  setInverse <- function(solveMatrix) j <<- solveMatrix  ##assigns value of inv in parent environment
  getInverse <- function() j
  ##in order to refer to the functions with the $ operator
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  j <- x$getInverse()
  if(!is.null(j)) {
    message("getting cached data")
    return(j)
  }
  data <- x$get()
  j <- solve(data, ...)
  x$setInverse(j)
  j
}
