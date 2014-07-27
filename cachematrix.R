## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  lInv <- NULL
  set <- function(y) {
    x <<- y
    lInv <<- NULL
  }
  get <- function() {x}
  setInv <- function(inv) {lInv <<- inv}
  getInv <- function() {lInv}
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
          lInv <- x$getInv()
  if(!is.null(lInv)) {
    message("getting cached data")
    return(lInv)
  }
  data <- x$get()
  lInv <- solve(data, ...)
  x$setInv(lInv)
  lInv
}
