## Calculates inverse for matrices

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL                 ## Initialize inverse as null
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x         ## Function retrieves matrix x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Retrieves cache data

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if(!is.null(inv)) {                   ##Checks if inverse is null
    message("getting cached data")
    return(inv)                         ##Returns inverse value
  }
  data <- x$get()
  inv <- solve(data, ...)               ##Calculates inverse value
  x$setInverse(inv)
  inv  
}
