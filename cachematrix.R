## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
      inverse <- NULL
      set <- function(y){
          x <<- y
          inverse <<- NULL
      }
      get <- function() x
      setinverse <- function(matrix_inverse) inverse <<- matrix_inverse
      getinverse <- function() inverse
      list(set = set,get = get,setinverse = setinverse,getinverse = getinverse)
      
  
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
      inverse <- x$getinverse()
      if(!is.null(inverse))
      {
        message("Getting cache value")
        return(inverse)
      }
      data <- x$get()
      inverse <- inv(t(data))
      x$setinverse(inverse)
      inverse
}
