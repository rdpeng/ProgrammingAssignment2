## The makeCacheMatrix returns cache if it exists and resets values if 
## new input is provided to makeCacheMatrix. cacheSolve  returns inverse
## or cache value as appropriate

## Save and return cache

makeCacheMatrix <- function(x = matrix()) {
     inverse <- NULL
     set <- function(y) {
          x <<- y
          inverse <<- NULL
     }
     get <- function() x
     setInverse <- function(solve) inverse <<- solve
     getInverse <- function() inverse 
     list(set = set, get = get,
          setInverse = setInverse ,
          getInverse = getInverse )
}


## Calculate Inverse. Also returns cached value if this has been 
##calculated earlier

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	m <- x$getInverse ()
     if(!is.null(m)) {
          message("getting cached data")
          return(m)
     }
     data <- x$get()
     m <- solve(data, ...)
     x$setInverse (m)
     m
}
