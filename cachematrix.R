## the programming assignment is to cache the invertible matrix for later use without computed repeatedly(if not necessary). 
## this feature is inherited from the 'Scoping rule' of R.
## this

## the first function is to create a 

makeCacheMatrix <- function(x = matrix()) {

  inv <- NULL
  set <- function(y = matrix()) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(invMat) inv <<- invMat
  getinv <- function() inv
  list (set = set, get = get, setinv = setinv, getinv = getinv)

}

## the cacheSove will retrieve the inverted matrix in the cache and skips
## computation. Otherwise, it solve the inverted matrix of the data and 
## set to the inv in the cache via the setinv function

cacheSolve <- function(x, ...) {
      
        ## Return a matrix that is the inverse of 'x'
      inv <- x$getinv
      if (!is.null(inv)) {
        message ("getting cached data")
        return(inv)
      }
      data <- x$get()
      inv <- solve(data,...)
      x$setmean(inv)
      inv
}
