

## makeCacheMatrix is a function which could cache the inverse of a matrix as an input so that it could help us
## to save some time if we need to calculate the inverse of that matrix several times.

makeCacheMatrix <- function(x = matrix()) {

inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) 
  inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get, setinv = setinv, getinv = getinv)


}


## cacheSolve function helps us to calculate the inverse of a matrix and if the matrix defined in the
##makeCacheMatrix has not changed and it recalls the inverse from cache if it has been already calculated.

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
  if(!is.null(inv)) {
    message("it's cached result!")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
