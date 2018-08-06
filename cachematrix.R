## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# The function makeCacheMatrix creates list of four funtions:
# 1. Initialize the value of the matrix
# 2. Get value of the matrix
# 3. Cache inverse of the matrix
# 4. Get inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  #
  ## Set default inverse value
  #
  matrixInverse <- matrix() # only one element that will have value NULL
  #
  ## Function #1 Inialize():  Initializes/sets the value of the matrix to be cached
  #
  InializeMatrix <- function(y){
   x <- y # Cache matrix
   matrixInverse <- matrix() # Initialize inverse to be a null matrix for starters
  }
  #
  ## Function #2 getMatrix():  Initializes/sets the value of the matrix to be cached
  #  
  getMatrix <- function() x
  #
  ## Function #3 cacheInverse():  Caches inverse of the matrix provided as funtion argument
  #  
  cacheInverse <- function(inverse) matrixInverse <- inverse
  #
  ## Function #4 getInverse():  Fetches cached value of the inverse from the cache
  #
  getInverse <- function() matrixInverse
  #
  ## Create list of the four function objects that allow working with the cached matrix
  #
  List(initialize = InializeMatrix, get = getMatrix, cacheI = cacheInverse, getI = getInverse )
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse <- x$getInverse(); # Get value from the cache
  if (!is.null(inverse[1][1])) { # Did cache had previously stored value?
    message("getting cached data") # Yess... awesome, we return that, life is good
    return(inverse)
  }
  #
  ## This is first time for inverse of this matrix, so we go ahead and get the inverse.
  ## Cache the value, and also return it here.
  #
  matrix <- x$getMatrix() # Get the matrix
  inverse <- solve(matrix, ...) # Get its inverse
  x$cacheInverse(inverse) # Cache the calculate inverse
  inverse # Also return it
}
