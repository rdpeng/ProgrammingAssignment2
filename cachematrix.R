## Programming Assignment 2 - Lexical Scoping

## making the vector

makeCacheMatrix <- function(x = matrix()) {
	cinv <- NULL
	set <- function(y) {
		x <<- y
            cinv <<- NULL
      }
      get <- function() x
      setInv <- function(inv) cinv <<- inv
      getInv <- function() cinv
      list(set = set, get = get,
           setInv = setInv,
           getInv = getInv)
}


## getting the inverse

cacheSolve <- function(x, ...) {
	cinv <- x$getInv()
      if(!is.null(cinv)) {
      	message("getting cached data")
            return(cinv)
      }
      data <- x$get()
      cinv <- solve(data, ...)
      x$setInv(cinv)
      cinv
}
