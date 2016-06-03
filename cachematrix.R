##
## The two R programs below reduce time consuming CPU operations using cache to capture matrix inverstion results 
##   so that you can reuse over and over without having to recompute. 
##
## Matrix inversion is usually costly, especially when continually running inside a loop. 
##
## The following functions capture a square matrix, then compute and cache the inverse of that matrix.
##
## First Program:
##   The makeCacheMatrix program sets up a square matrix that will later be converted by the cacheSolve program.
##
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}
## Second Program:
##  The program, cacheSolve takes a square matrix, computes its inverse and saves it in cache.
##   If the inverse has already been calculated and the matrix has not changed, 
##    it’ll retrieves the inverse from the cache directly so as avoid having to recompute.
##
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
   inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv       
}
##
