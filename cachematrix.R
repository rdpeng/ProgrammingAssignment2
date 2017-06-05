## Caching the inverse of a matrix if the values are not changing.
## If they are changing,calculate the inverse

## makeCacheMatrix function,

makeCacheMatrix <- function(x = matrix()) {
  
  ## holds the cached value or NULL if nothing is cached
  ## initially nothing is cached so set it to NULL
  inv <- NULL
  
  ## store a matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  ## getting the matrix
  get <- function() x
  
  #set the inverse
  setInverse <- function(inverse) inv <<- inverse
  
  ## get inverse,cached value
  getInverse <- function() inv
  
  ## the function returns a list,which has its named elements as functions.
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## calculating the inverse of matrix

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
        
}
