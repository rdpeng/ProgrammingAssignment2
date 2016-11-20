## These functions inverts matrices
## If the inverse is already found out, then they do not re-compute the inverse

## This function returns a list with the functions required for the other function
## To create the inverse of a matrix. It takes the source matrix as the argument,
## Initializes the parameters and returns a list

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      set <- function(y) {
            x <<- y
            inv <<- NULL
      }
      get <- function() x
      setinv <- function(inverse) inv <<- inverse
      getinv <- function() inv
      list(set = set, get = get,
           setinv = setinv,
           getinv = getinv)

}


## This functions checks if the inverse is currently present for the matrix that was
## Created in the previous function. It takes the list as argument and computes the inverse only
## If it is non-existing

cacheSolve <- function(x, ...) {
      inv <- x$getinv()
      if(!is.null(inv)) {
            message("getting cached data")
            return(inv)
      }
      data <- x$get()
      inv <- solve(data, ...)
      x$setinv(inv)
      inv        ## Return a matrix that is the inverse of 'x'
}
