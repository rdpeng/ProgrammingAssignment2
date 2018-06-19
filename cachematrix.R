## This function computes the inverse of the special "matrix"
## returned by makeCacheMatrix above.

## calculates matrix inverse

makeCacheMatrix <- function(x = matrix()) {

}


## used to calculate matrix inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
}
inv <- x$getInverse()
if(!is.null(inv)){
  message("getting cached data")
  return(inv)
}
data <- x$get()
inv <- solve(data)
x$setInverse(inv)
inv      
}