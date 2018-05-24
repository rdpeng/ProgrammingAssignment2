## Put comments here that give an overall description of what your
## functions do

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL				##initiazing m as NULL, it will contain the inverse of the matrix
	set <- function(y) {			##set function is defined here
		x <<- y				##setting value of matrix in parent environment
            m <<- NULL				##setting m to NULL
      }
      get <- function() x			##get function is defined here
      setinv <- function(inv) m <<- inv		##value of m is assigned in parent environment
      getinv <- function() m			##gets the value of m in the environment called
      list(set = set, get = get,
           setinv = setinv,
           getinv = getinv)			##to allow $ operations we need list
}

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
}
