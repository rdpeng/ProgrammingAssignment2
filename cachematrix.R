## Input a matrix 'A' into this fn using B <- makeCacheMatrix(A)
## If the inverse was already solved, B$getInverse() will retrieve it
## This is a function that actually creates four functions:
##       1. B$get() ==> retrieves the matrix
##       2. B$getInverse() ==> retrieves the inverse (if available)
##       3. B$setInverse() ==> caches the inverse
##       4. B$set ==> initialize the inverse, exchanges values
## Works in conjunction with fn cacheSolve


makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {                             # initialize inverse 
    x <<- y
    inv <<- NULL
  }
  get <- function() x                              # function that gets the matrix
  setInverse <- function(inverse) inv <<- inverse  # function that caches the inverse
  getInverse <- function() inv                     # function that gets inverse         
  list(set = set, get = get,                       # four functions arranged in a list
       setInverse = setInverse,
       getInverse = getInverse)
}



## Works in conjunction with makeCacheMatrix
## Checks if inverse was cached, if not calculates it and returns it
## If cached, returns cached value

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()                            # locate already obtained inverse
  if(!is.null(inv)) {                              # if you got it...
    message("getting cached data")
    return(inv)                                    # return it
  }
  mat <- x$get()                                   # otherwise get the matrix created by makeCacheMatrix
  inv <- solve(mat, ...)                           # calculate its inverse
  x$setInverse(inv)                                # park value for future retrieval
  inv                                              # return the calculated inverse
}
