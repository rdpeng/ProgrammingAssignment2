## Put comments here that give an overall description of what your
## functions do
# I created this function to be capable of using a valid input matrix to create a special matrix that can recycle data if it has been generated previously. 

## Write a short comment describing this function
## There are two functions, makeCahceMAtrix consists of set, get, setinverse, and getinverse. 
## It generates a special matrix object that can cache the inverse.

makeCacheMatrix <- function(x = matrix()) {
  
                    invrs <- NULL
                      set <- function(y) {
                                           x <- y
                                          invrs <- NULL
  }
                      
          get <- function() x
    setinverse <- function(inverse) invrs <<- inverse
    getinverse <- function() invrs
                  list(set = set, get = get,
                      setinverse = setinverse,
                      getinverse = getinverse)
}


## Write a short comment describing this function
## this function computes the inverse of the special matrix returned by makeCacheMatrix above

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
}
cacheSolve <- function(x, ...) {
  invrs <= x$getinverse()
        
        if(!is.null(invrs)) {
            message("getting cached data")
    return(invrs)
  }
    matrix_to_invert <- x$get()
               invrs <- solve(matrix_to_invert, ...)
                          x$setinverse(invrs)
                                  invrs
}

