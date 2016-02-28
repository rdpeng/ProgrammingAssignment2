## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
## The process initialize the inverse property 
        inv <- NULL
        ## Process to set the matrix
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        
## Process the get the matrix
        get <- function() x
        ## Process to set the inverse of the matrix
        setInverse <- function(inverse) inv <<- inverse
        ## Process to get the inverse of the matrix
        getInverse <- function() inv
        ## Return a list of the processes facts
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        ## Return the inverse if its already set
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        ## Get the matrix from the data 
        mat <- x$get()
        ## Use matrix multiplication to get the inverse 
        inv <- solve(mat, ...)
        x$setInverse(inv)
        inv
}
