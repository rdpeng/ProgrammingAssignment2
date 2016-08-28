##This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  ## Make a special matrix which is a list containg a function
  Invrs <- NULL
  set <- function(y) {
    ## set the value of the mattrix
    x <<- y
    Invrs <<- NULL
  }
  get <- function() x
  ## get the value of the matrix
  setinverse <- function(inverse) Invrs <<- inverse
  ## set the inverse value of the matrix 'x'
  getinverse <- function() Invrs
  ## get the inverse value of the matrix 'x'
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

##his function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of matrix 'x'
  Invrs <- x$getinverse()
  ## Checks to see if the inverse has alredy been calculated
  if(!is.null(Invrs)) {
    message("getting cached data")
    return(Invrs)
    ## If so it gets the inverse from the cache and skips the computation
  }
  data <- x$get()
  Invrs <- solve(data, ...)
  x$setinverse(Invrs)
  ## Calculates the inverse of the matrix and 
  ## set the value of the inverse 
  Invrs
}