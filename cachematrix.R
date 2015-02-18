# makeCacheMatrix creates a list containing a function to
# set the value of the matrix, get the value of the matrix
# set the value of inverse of the matrix, get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x<<-y
    inv <<- NULL
  }
  get <- function () x
  setinverse <- function(inverse) inv<<-inverse
  getinverse <- function() inv
  list (set=set, get=get, setinverse=setinverse, getinverse=getinverse)

}


# Returns the inverse of the matrix. It first checks if it has already been computed. 
# Yes - gets the result & skips calcultion.
# No - computes the inverse, sets the value in the cache.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)){
    message ("Fetching cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}
