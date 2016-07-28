# This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  # initialize cache to NULL
  cache <- NULL
  
  # set the values of x and cache in the working environment
  set <- function(y) {
    x <<- y
    cache <<- NULL
  }
  
  # get the value of the matrix x
  get <- function() x
  
  # set the value of cache to be the inverted matrix
  setInverse <- function(inverse) cache <<- inverse
  
  # get the inverted matrix from cache
  getInverse <- function() cache
  
  # name and return the functions to the working environment
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)

}




# This function calculates the inverse of the special "matrix" created
# with makeCacheMatrix. If the matrix has already been calculated, it 
# gets the inverse from the cache and skips the computation. Otherwise, 
# it calculates the inverted matrix and sets the value of the inverted 
# matrix in the cache.

cacheSolve <- function(x, ...) {
  
    # get result from makeCacheMatrix and set initial cache
    cache <- x$getInverse()
    
    # check if cache is already calculated 
    if (!is.null(cache)) {
              message("getting cached data")
              # return stored inverted matrix in console
              return(cache)
    }
  
    # create matrix 
    matrix <- x$get()
    
    # calculate and return inverse of matrix
    cache <- solve(matrix, ...)
    
    # set inverted matrix in cache
    x$setInverse(cache)
    
    # return inverted matrix in console
    return (cache)
}

