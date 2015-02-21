## Create and cache a matrix and its inverse

## Create the matrix that can be used in cacheSolve. Gets and Sets the values.
makeCacheMatrix <- function(someMatrix = matrix()) {
  
  invertedMatrix <- NULL
  set <- function(y) {
    someMatrix <<- y
    invertedMatrix <<- NULL
  }
  get <- function() someMatrix
setInverse <- function(solve) invertedMatrix <<- solve
getInverse <- function() invertedMatrix

list(set = set, get = get,
     setInverse = setInverse,
     getInverse = getInverse)
}

## Compute inverse unless it has already been created.
## If there is no change, return the original

cacheSolve <- function(cacheMatrix, ...) {
  invertedMatrix <- cacheMatrix$getInverse()
##No change? Get the Cache!  
  if(!is.null(invertedMatrix)) {
    message("Let me get that cached matrix for you")
    return(invertedMatrix)
  }
  matrixInvert<- cacheMatrix$get()
  invertedMatrix <-solve(matrixInvert)
  cacheMatrix$setInverse(invertedMatrix)
  invertedMatrix
}
##Thank You!!

