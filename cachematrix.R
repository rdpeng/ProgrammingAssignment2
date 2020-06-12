## makeCacheMatrix creates a special "matrix" object that can cache its inverse

## cacheSolve computes the inverse of the special "matrix" returned by 
## makeCacheMatrix. If the inverse has already been calculated and the matrix
## has not changed, cacheSolve retrieves the inverse from the cache.



## Create a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      set <- function(y) {
            x <<- y
            inv <<- NULL #sets inv back to NULL when matrix is changed
      }
      
      get <- function() x
      setinverse <- function(inverse) {
            inv <- inverse
      }
      
      getinverse <- function() inv
      list(set = set, get = get, setinverse = setinverse, 
           getinverse = getinverse)
}



## Retrieve or compute the inverse of the special "matrix" returned by 
## makeCacheMatrix.

cacheSolve <- function(x, ...) {
      inv <- x$getinverse()
      if(!is.null(inv)) {      #check if inverse cached for this matrix already
            message("Getting cached data.")
            return(inv)
      }
      
      data <- x$get()
      inv <- solve(data, ...) #calculate the inverse
      x$setinverse(inv)
      inv
}
