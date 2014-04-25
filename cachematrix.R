## A pair of functions that cache the inverse of a matrix, for the sake of saving time of 
## re-calcuating the result on subsequent calls.

## Creates a special wrapper around given matrix, 
## which offers a quick (cached) computation of matrix inverse
makeCacheMatrix <- function(x = matrix()) {
   matrix <- x
   inverse <- NULL

   getCachedInverse <- function() inverse
   getMatrix <- function() matrix
   calculateInverseAndStore <-function() {
     inverse <<- solve(matrix)
     inverse
   }
   list(getCachedInverse = getCachedInverse, calculateInverseAndStore = calculateInverseAndStore)
}


## Returns inverse of a wrapped matrix created with makeCacheMatrix.
## Ensures the matrix inverse is calculated only once and cached for subsequent calls.
cacheSolve <- function(cacheMatrix, ...) {
  candidate <- cacheMatrix$getCachedInverse()
  if (!is.null(candidate)) {
    message("from cache")
    return (candidate)
  }

  cacheMatrix$calculateInverseAndStore()
}
