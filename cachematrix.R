## Caching the Inverse of a Matrix useing makeCacheMatrix 


makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inverse <<- inverse
        getInverse <- function() inverse
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}

## cacheSolve computes the inverse of a special "matrix" returned by the makeCacheMatrix

cacheSolve <- function(x, ...) {
        inverse <- x$getInverse()
	  if (!is.null(inverse)){
		message("getting cache data")
		return(inverse)
	  }
 	  matrix <-x$get()
	  inverse <-solve(mat, ...)
	  x$setInverse(inverse)
	  inverse
}

