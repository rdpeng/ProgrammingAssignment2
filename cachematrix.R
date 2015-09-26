## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        cache <- NULL
        
        # create the matrix in the working environment
        set <- function(y) {
                x <<- y
                cache <<- NULL
        }
        
        # get the value of the matrix
        get <- function() x
        # invert the matrix and store in inv
        setInverse  <- function(inverse) cache <<- inverse
        # get the inverted matrix from cache
        getInverse <- function() cache
        
        # return the created functions to the working environment
        list(set = set, get = get,
             setInverse  = setInverse ,
             getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        cache <- x$getInverse()
        if(!is.null(cache)) {
                message("getting cached data.")
                return(cache)
        }
        data <- x$get()
        cache <- solve(data)
        x$setInverse (cache)
        cache
        
        
}
