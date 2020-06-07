## Put comments here that give an overall description of what your
## functions do
## the functions in this code will use the concept of lexical scoping to reduce the time taken to do computaional operations on exceedingly long amount times, such as for instance in carrying out a loop over a lonf list, 
## this will be done by the makeCacheMatrix and the cacheSolve function, these functions will specifically make it easier to carry out the operation of getting inverse of a matrix when doing it over a long loop, for instance.

## Write a short comment describing this function
## the makeCacheMatrix will create a special matrix, which is actually a long list containing a function to set the values of the matrix, get the value of the matrix, set the value of the inverse of the matrix, and get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function(){x}
  setInverse <- function(inverse) {inv <<- inverse}
  getInverse <- function() {inv}
  list(set = set,get = get,setInverse = setInverse,getInverse = getInverse)
}


## Write a short comment describing this function
## the cacheSolve function will get the inverse of the special matrix created with the above function, it first checks to see if the inverse of the matrix has already been calculated.it the inverse has been calculated, it will get the inverse of the matrix from the cacheand skips the computation. if the inverse of the matrix has not been calculated, it calculates the inverse of the matrix and sets the value of the mean in the cache via the setInverse function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(!is.null(inv)){
    message("getting cacahed data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}
