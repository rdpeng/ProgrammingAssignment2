# Example usage:
	# > a <- matrix(rnorm(12), nrow = 4) // Create a matrix a
	# > y <- makeCacheMatrix(a) // Create own special matrix
	# > y$get() // Return the matrix
	# > cacheSolve(y) // Return the inverse
	# > cacheSolve(y) // Call the 2nd time, so return the cached inverse

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse 
	makeCacheMatrix <- function(x = matrix()) {
	  i <- NULL
	  set <- function(y){
	    x <<- y
	    i <<- NULL
	  }
	  get <- function() x
	  setinverse <- function(inverse) i <<- inverse
	  getinverse <- function() i
	  list(set= set, get = get,
	       setinverse = setinverse,
	       getinverse = getinverse)

	}

##cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

	cacheSolve <- function(x, ...) {
	  i <- x$getinverse()
	  if (!is.null(i)){
	    message("getting cached data")
	    return(i)
	  }
	  data <- x$get()
	  i <- solve(data, ...)
	  # Cache the inverse
	  x$setinverse(i)
	  # Return 
	  i
	}