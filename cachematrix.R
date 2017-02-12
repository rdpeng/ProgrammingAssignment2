## The following two functions take advantage of the scoping rules that preserve computation results 
## inside of an R object. It is effective to perform iterative computation by lookeding up 
## the results in the cache.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  ## Initialize the value of the inverse to NULL
  ivs <- NULL  
  
  ## Set function sets the passed values to 'x' and NULL to its inverse
  set <- function(y) {
    x <<- y
    ivs <<- NULL
  }
  
  ## Get function returns the values of 'x'
  get <- function() {
    x
  }
  
  ## setInverse function sets the passed value to the inverse
  setInverse <- function(ivsMatrix) {
    ivs <<- ivsMatrix
  }
  
  ## getInverse function returns the inverse
  getInverse <- function() {
    ivs
  }
  
  ## each function of makeCacheMatrix is stored in the list element  
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix

cacheSolve <- function(x) {
  
  ## Get x's inverse and store it in 'ivs'
  ivs <- x$getInverse()
  
  ## If the inverse value has been already computed, just return it and exit
  if (!is.null(ivs)) {
    
    message("getting cached data...")
    return(ivs)
    
  }
  
  ## If the inverse value is NULL, get matrix, compute its inverse, set the computed value
  ## and return it.
  
  matrix <- x$get()
  ivs <- solve(matrix)
  x$setInverse(ivs)
  
  return(ivs)
}
