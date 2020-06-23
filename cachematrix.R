## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix 
## rather than compute it repeatedly (there are also alternatives to matrix inversion that we will not discuss here). 

## this function creates a special "matrix" object that can cache its inverse


makeCacheMatrix <- function(x = matrix()) {
  
  invert <- NULL
  
  #creating another function
  set <- function (y) {
    x <<- y 
    invert <<- NULL
  }
  
  get <- function() {x} #get the value of matrix
  set_Inverse <- function(inverse) {invert <<- inverse} #set the inverse
  get_Inverse <- function() {invert} #get the inverse
  
  list(set = set, 
       get = get, 
       set_Inverse = set_Inverse, 
       get_Inverse = get_Inverse)
  
}


## Return a matrix that is the inverse of 'x'
## this function calculates the inverse of the special "matrix" returned by makeCacheMatrix above

cacheSolve <- function(x, ...) {
  
  invert <- x$get_Inverse()
  
  if(!is.null(invert)) {
    message("getting cached data")
    return(invert)
  }
  
  mat <- x$get()
  invert <- solve(mat, ...)
  x$set_Inverse(invert)
  invert
  
}
