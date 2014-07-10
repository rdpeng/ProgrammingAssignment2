## Date: 10-JUL-2014
## This program accepts a matrix as input and returns its inverse from cache 
## is it exists or computes it if the inverse does not exist in cache

## The makeCacheMatrix function is a constructor function accepts matrix as the input and 
## returns a list of 4 functions as the output. The 4 functions are set, get,
## setInverse and getInverse

makeCacheMatrix <- function(x = matrix()) {

        m <- NULL ##Initialize the value to NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInverse <- function(Inverse) m <<- Inverse  ## Function to inverse the matrix
        getInverse <- function() m
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)   ## The list object that is being returned
        
}


## This function accepts the matrix as input, calls the makeCacheMatrix function
## to create the Inverse of a matrix and retrieves from cache if it already exists.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getInverse()
        if(!is.null(i)) {   ##Validating if the data exists in the cache
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setInverse(i)
        i
}
