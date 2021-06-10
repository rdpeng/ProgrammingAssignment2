## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
   
 
  
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getreverse()
    if(!is.null(inv)) {
      message("Get cache data.")
      return(inv)
    }else{
      data <- x$get()
      inv <- solve(data)
      x$setreverse(inv)
      inv
  }
  
 
  
        ## Return a matrix that is the inverse of 'x'
}
