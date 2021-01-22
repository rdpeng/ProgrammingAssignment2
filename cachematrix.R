## The following function creates a matrix object that 
## can cache its inverse


makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      set <- function(y){
            x <<- y
            inv <<- NULL
      }
      get <- function() {x}
      setinverse <- function(inverse) {inv <<- inverse}
      getinverse <- function() {inv}
      list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## The following function is capable of computing the 
## inverse of the matrix returned by the above function.
## If the inverse is already calculated then the function
## retrieves the same from the cache.

cacheSolve <- function(x, ...) {
      inv <- x$getinverse()
      if(!is.null(inv)){
            message("fetching data from cache")
            return(inv)
      }
      mat <- x$get()
      inv <- solve(mat, ...)
      x$setinverse(inv)
      inv
}
