## below are two functions, one calculate the inverse of the Matrix and cached it, and
## the other solve matrix for its inverse, if it's cached it will call it directly, if
## not cached will calculate it.

## makeCacheMatrix will take Matrix as an argument and calculate the inverse and cached it

makeCacheMatrix <- function(x = matrix()) {
  
   inverseofx <<- solve(x) 
   return(x)
}


## cacheSolve will either call the inverse of the Matrix from the cache or calculate and
## cached it.

cacheSolve <- function(y, ...) {
        ## Return a matrix that is the inverse of 'x'
  I2 <- matrix(c(1,0,0,1),nrow = 2,ncol = 2)
  if (all.equal(y %*% inverseofx,I2) == TRUE) {
    message("getting cached data")
    return(inverseofx)
  }
  inverseofx <<- solve(y)
  inverseofx
}

