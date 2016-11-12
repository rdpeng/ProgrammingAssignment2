## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {    x <<- y                    ## the function is pretty much the same
                        inv <<- NULL  }              ## just changed mean by inverse    
  get <- function() x                                ## main learn is that when an object  
  setInverse <- function(inverse) inv <<- inverse    ## is not found in func env it looks at parents
  getInverse <- function() inv                       ## env, the 'y' in this case.
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {                    ## same here, changing m/mean
  ## Return a matrix that is the inverse of 'x'     ## by inverse/solve
  i <- x$getInverse()
  if (!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  matr <- x$get()
  i <- solve(matr, ...)
  x$setInverse(i)
  i 
}

