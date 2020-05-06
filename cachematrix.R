## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## Creates a matrix with some functions for accessing and changing it and the inverse
makeCacheMatrix <- function(x = matrix()) {
      i = NULL
      set <- function(y){
            x <<- y
            i = NULL
      }
      
      get <- function() x 
      setInverse <- function(inverse) {i <<- inverse}
      getInverse <- function() i

}


## Write a short comment describing this function
## This function checks to see if the inverse has already been calculated. 
## If so it returns the inverse otherwise it calculates the new inverse. 
cacheSolve <- function(x, ...) {
      i <- x$getInverse()
      if(!is.null(i)){
            message("getting data")
            i
      }
      
      i <- solve(x$get())
      x$setInverse(i)
      i
}
