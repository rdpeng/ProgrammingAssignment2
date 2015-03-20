## create a solvable matrix using this function
## create the matrix using makeCacheMatrix()
## to execute:
## a <- makeCacheMatrix( matrix(c(1:4), nrow = 2, ncol = 2) );
## cacheSolve(a)

## in run 1, the inverse is calculated, secind time around, the value from cache is called.

# setMatrix      set the value of a matrix
# getMatrix      get the value of a matrix
# cacheInverse   set the inverse of the matrix into cache
# getInverse     get the inverse of the matrix from cache
#

makeCacheMatrix <- function(x = numeric()) {
  
    ## init cache
    m <- NULL
    
    # store incoming matrix in x
    setMatrix <- function(y) {
      x <<- y
      m <<- NULL
    }
    
    # return the stored matrix
    getMatrix <- function() { 
      x
    }
    
    # cache the given argument (in htis case solved matrix)
    cacheInverse <- function(solve) {
      m <<- solve
    }
    
    # get the cached value
    getInverse <- function() {
      m
    }
    
    # return a list. Each named element of the list is a function
    list(setMatrix = setMatrix, 
         getMatrix = getMatrix, 
         cacheInverse = cacheInverse, 
         getInverse = getInverse)
}


## Solves the matrix created by the makeCacheMatrix function

cacheSolve <- function(y, ...) {
  # get the cached value
  inverse <- y$getInverse()
  
  # if a cached value exists return it
  if(!is.null(inverse)) {
    message("Cached data")
    return(inverse)
  }
  
  # otherwise get the matrix, caculate the inverse and store it in
  # the cache
  data <- y$getMatrix()
  inverse <- solve(data)
  y$cacheInverse(inverse)
  
  # return the inverse
  return(inverse)
}
