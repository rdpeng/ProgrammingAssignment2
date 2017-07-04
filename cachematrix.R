## The first function makes a list with methods that set and get a matrix and its inverse in an intrinsic environment variable
## The second function is passed the list from the first and attempts to calculate and set its inverse.  
##If the inverse is already set, then the cached value is used


## This function creates a special "matrix" x that can cache its inverse
## The function will expose three methods to set/get x and its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  	set <- function(y) {
    		x <<- y
    	  inv <<- NULL
    	}
  	get <- function() x
    setinv <- function(invers) inv <<- invers
    getinv <- function() inv
    list(set = set, get = get, setinv = setinv, getinv = getinv)
  }
  


## This function computes the inverse of the special matrix returned by "makeCacheMatrix" above.
## If the inverse has already been calculated (and the matrix has not changed), then the`cachesolve` 
## should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}


