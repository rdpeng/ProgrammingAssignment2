## writing a pair of functions that cache the inverse of a matrix
## creating a special "matrix" object that can cache its inverse
## creating a function to compute the inverse of the special "matrix" 

## This function creates a special matrix that caches its inverse

makeCacheMatrix <- function(x = matrix()) {
              i <- NULL
              set <- function(y) {
                x <<- y
                i <<- NULL
              }
              get <- function() x
              setinverse <- function(inverse) i <<- inverse
              getinverse <- function() i
              list(set = set, get = get,
                   setinverse = setinverse,
                   getinverse = getinverse)  
}


## This function computes the inverse of the special "matrix" created by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then it retrieves the inverse from the cache.


cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) {
          message("getting cached data")
          return(i)
        }
        mat <- x$get()
        i <- solve(mat, ...)
        x$setinverse(i)
        i
}
