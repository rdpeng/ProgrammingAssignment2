## This function represents a class that creates an Object that stores a Matrix
#It's possible to set the value of a Matrix within 2 different ways:
## obj1 <- makeCacheMatrix(initialMatrix) ## pe.: matrix(c(1, -0.25, 3, -0.25, 1, 6, 1.4, 4, 8), 3, 3)
## obj2 <- makeCacheMatrix()
## obj2 <- obj$set(initialMatrix)
##The object can be manipulated by cacheSolve and then, returns 4 lambdas:
##  set - changes the value of the original matrix (Constructor)
##  get - gets the value of the original matrix
##  getInverse - gets the cached inverse of the matrix once set
##  setInverse - sets the cached inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(pValue) {
    x <<- pValue
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(pValue) inv <<- pValue
  getInverse <- function() inv
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## If the object has a inversed matrix in its cache, just show the cached
## otherwise, the function calculates its inverse, cache the value and then
## show the value
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  cache <- x$getInverse()
  if(!is.null(cache)) {
    message("getting cached data")
    return(cache)
  }
  
  originalData <- x$get()
  rValue <- solve(originalData, ...)
  x$setInverse(rValue)
  rValue
}
