##  A pair of functions that cache and compute the inverse of a matrix.

##  This function creates a special matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(x) {
    x <<- x;  #  Save cached matrix in this superassignemnt variable (x <<-)
    inverse <<- NULL;  #  Check that inverse is still "NULL"
  }
  get <- function() return(x);
  setinv <- function(inv) inverse <<- inv;
  getinv <- function() return(inverse);
}

##  This function computes the inverse of the special matrix returned 
##  by `makeCacheMatrix` above. If the inverse has already been calculated 
##  (and the matrix has not changed), then `cacheSolve` will return the 
##  inverse of the matrix from the cache.

cacheSolve <- function(x, ...) {
  inverse <- solve(x) #  name inverse "inverse" of matrix x, solve for inverse
  if(!is.null(inverse)) { #  If matrix is NOT NULL, get inverse
    message("Getting cached data...")  #  Print message
    return(inverse)
  }
  data <- x$get()   #  get() gets value of x (x$get())
  invserse <- solve(data, ...)  #  get inverse of x
  x$setinv(inverse)  #  Name inverse of matrix "x"
  return(inverse)    #  keep inverse
}
