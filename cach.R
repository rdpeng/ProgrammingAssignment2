##  cachematrix.R
##  Michael Scherrer, 16 July 2014
##
##  Set of functions that can store and recall a cached version
##  of a square matrix and its inverse matrix.

##  USAGE
##  
##  > mat <- matrix(c(1:4),c(2,2))
##  > cacheMat <- makeCacheMatrix(mat)
##  > cacheMat$get()
##       [,1] [,2]
##  [1,]    1    3
##  [2,]    2    4
## > cacheSolve(cacheMat)
##       [,1] [,2]
##  [1,]   -2  1.5
##  [2,]    1 -0.5
##  > cacheSolve(cacheMat)
##  Retreiving the cached inverse matrix...
##       [,1] [,2]
##  [1,]   -2  1.5
##  [2,]    1 -0.5

##  Creates an object with the cache of a square matrix and creates
##  a list of functions for interacting with the matrix.
##
##  - set        Creates a cache of the matrix upon the call of
##               makeCacheMatrix and clears any previous cached
##               inverse matrix
##  - get        Returns the cached matrix
##  - setinv     Creates a cache of the solved inverse of the
##               orignal cached matrix created by set
##  - getinv     Returns the cached inverse matrix

makeCacheMatrix <- function(x = matrix()) {

  i <- NULL
  
  set <- function(y){
    x <<- y 
    i <<- NULL  
  }
  
  get <- function() x
  
  setinv <- function(inv) i <<- inv
  
  getinv <- function() i
  
  list(set = set,
       get = get,
       setinv = setinv,
       getinv = getinv)    
}


##  Solves the inverse of the cached square matrix created by
##  makeCacheMatrix above. If the inverse of the cached matrix
##  has already been solved, returns the cached version of the
##  inverse matrix. Otherwise, solves the inverse matrix and
##  stores it in the cache.

cacheSolve <- function(x, ...) {
  
  i <- x$getinv()

  if(!is.null(i)) {
    message("Retreiving the cached inverse matrix...")
    return(i)
  }
  
  m <- x$get()
  i <- solve(m, ...)
  x$setinv(i)
  i
}
