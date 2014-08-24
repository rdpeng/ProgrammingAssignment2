## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL	# Set variable i as NULL  
        set <- function(y) { 
                x <<- y
                i <<- NULL
        }
        get <- function() x

	  # A function of setting an inverse matrix
        setinverse <- function(inverse) i <<- inverse

	  # A function of getting an inverse matrix
        getinverse <- function() i
	  
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse() # Get an inverse matrix saved in the list 'x'

        if(!is.null(i)) { # Print a message if the cache already has the output
                message("getting cached data")
                return(i)
        }
        data <- x$get()
	  # Calculate the inverse matrix of matrix 'data'
        i <- solve(data, ...)

	  # Set the inverse matrix 'i' into cache
        x$setinverse(i)
        i        
}
