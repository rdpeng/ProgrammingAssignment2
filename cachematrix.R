##Functions to inverse a matrix caching data.

##This function creates a special "matrix" object that 
##can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {

        inverse_matrix <- NULL

	  ##set the value of the matrix
	  ##initialize the inverse matrix
	  set <- function(y) {
                x <<- y
                inverse_matrix <<- NULL
        }

	  ##get the value of the matrix
        get <- function() {
	  	    x
	  }

	  ##set the value of the inverse matrix
        setinverse <- function(solve) {
                inverse_matrix <<- solve
	  }

	  ##get the value of the inverse matrix
        getinverse <- function() {
                inverse_matrix
	  }

	  ##form the returned list
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

##This function computes the inverse of the special "matrix" returned 
##by makeCacheMatrix above. If the inverse has already been calculated 
##(and the matrix has not changed), then the cachesolve retrieves the 
##inverse from the cache.
cacheSolve <- function(x, ...) {

	  ##Get the inverse matrix (if already calculated)	  
        i <- x$getinverse()

	  ##If the data is not null, the inverse matrix
 	  ##had been already calculated and the data
        ##is returned
        if(!is.null(i)) {
                message("return data from cache memory")
                return(i)
        }

	  ##If it hadn't been calculated get the initial matrix.
        data <- x$get()

	  ##After getting the initial matrix, its inverse is calculated.
        i <- solve(data, ...)

	  ##The inverse is saved in cache memory.
        x$setinverse(i)

	  ##The inverse matrix is returned.
        i
}
