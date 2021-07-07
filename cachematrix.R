

## The makeCacheMatrix function takes a matrix input and assigns it using the set function within the function. This matrix can be fetched with $get()

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() {x}
  setinv <- function(solve) {inv <<- solve}
  getinv <- function() {inv}
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)

}


## This function takes the list of function produced from makeCacheMatrix and solves the inverse of the matrix. If the matrix has already been solved (or assigned using $setinv) then the function will output the cached value

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
