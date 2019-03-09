## the function makeCacheMatrix creates a special "matrix" and the function
## cacheSolve compute the inverse of the special matrix (if its content is null)
## or cache it via the first function if the inverse matrix has already been computed
#The first function, `makeCacheMatrix` creates a special "matrix", which is
#really a list containing a function to:
#1.  set the value of the matrix
#2.  get the value of the matrix
#3.  set the value of the inverse matrix
#4.  get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  inv_mat <- NULL
  set <- function(y) {
    x <<- y
    inv_mat <<- NULL
  }
  get <- function() x
  setinv <- function(inv) inv_mat <<- inv
  getinv <- function() inv_mat
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}
#The following function, cacheSolve calculates the invrse matrix 
#of the special "matrix"created with the above function(makeCacheMatrix). 
#However, it first checks to see if the inverse matrix has already been calculated.
#If so, it `get`s the inverse from the cache and skips the computation.
#Otherwise, it calculates the inverse matrix of the data and sets the value of 
#the inverse matrix in the cache via the `setinv`function.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv_mat <- x$getinv()
  if(!is.null(inv_mat)) {
    message("getting cached data")
    return(inv_mat)
  }
  data <- x$get()
  inv_mat <- solve(data, ...)
  x$setinv(inv_mat)
  inv_mat
}

