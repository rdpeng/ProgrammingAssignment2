## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

##function to find the inverse of matrix

##initially the value of inv is NULL, then we 
## set the matrix, using the  calculated inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function(){
    x 
  } 
  setinv <- function(inverse) {
    inv <<- inverse 
  }
  getinv <- function() {
    inv 
  }
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
  

}


## Write a short comment describing this function

##this is for calculating the inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {

    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
