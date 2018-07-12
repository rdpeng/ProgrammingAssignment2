## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  Inv <- NULL
  set <- function(y) {
    x <<- y
    Inv <<- NULL
  }
  get <- function() x
  setinv <- function(ginv) Inv <<- ginv
  getinv <- function() Inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## Return a matrix that is the inverse of 'x'
  Inv <- x$getinv()
  data <- x$get()
	if(!is.null(Inv)) {
		message("getting cached data")
		return(Inv)
	}else {
		Inv <- solve(data)
		x$setinv(Inv)
		return(Inv)
	}
}
