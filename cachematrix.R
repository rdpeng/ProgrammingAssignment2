## Put comments here that give an overall description of what your
## functions do
##
## Given a matrix-x0, the function computes and catches its inverse. So, given a second
## matrix-x rather then compute its inverse right away, first it compares matrix-x with matrix-x0 to be
## equal. If they are equals, return the catched inverse value, and if they are not then compute
## the inverse of matrix-x, saved and return the new value.
##
## Write a short comment describing this function
## To comppute the inverse of a matrix, it must be square and its determinant is different of zero.
## a) Make sure that x is a matrix, and it is of the square type.
## b) Make sure that x is a inverse matrix (determinant different of zero).
## c) Create a "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  if(!is.matrix(x)) stop("x must be a matrix")
  if(nrow(x) != ncol(x)) stop("x is not a square matrix.")
  if(det(x) == 0) stop("x doesn't have inverse; its determinant is zero")
  
  inv0 <- NULL
  set <- function(y){
    x <<- y
    inv0 <<- NULL
  }
  get <- function() x    
  setmean <- function(inv) inv0 <<- inv 
  getmean <- function() inv0
  list(set = set, get = get, setmean = setmean, getmean = getmean)  

}

## Write a short comment describing this function
## d) If the inverse has been calculated (inv0 != 0) and the matrix has not changed (x = x0),
##    the cacheSolve return the inverse from the cache (inv0).
## e) If the inverse has not been calculated (inv0 = NULL) or the matrix has changed (x != x0)
##    the cacheSolve computes the inverse (solve(x)%*%x), caches and returns the inverse (inv)

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getmean()
  if ((!is.null(inv)) & (x$set == x$get)) {    
    message("getting cached data")
    return(inv)
  } 
  data <- x$get()
  inv <- solve(data)%*%data(data, ...)
  x$setmean(inv)
  inv
}
