## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  # cached inverse of matrix
  inv <- NULL
  
  ## getter/setter for matrix
  get <- function() x
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  ## getter/setter for matrix inverse
  getinv <- function() inv
  setinv <- function(inverse) inv <<- inverse
  
  ## return list of functions for matrix
  list(get=get, set=set, getinv=getinv, setinv=setinv)
}

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
			m <-x$getinv()
			if(!is.null(m)){
				message(" getting  cached data")
				return(m)
			}
			data <- x$get()
			m <- solve(data, ...)
			x$setinv(m)
			m
	}
