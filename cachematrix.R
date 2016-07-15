## Below are a pair of functions that are used to create a special object that 
## stores a matrix and caches its inverse.

## This function creates a special “matrix” object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    
    inv <- NULL
    
    ## sets values of the matrix 
    ##`<<-` is used to assign a value to an object in an environment 
    # different from the current environment.
    
    set <- function(y){
        x <<- y
        inv <<- NULL
    }
    
    ## retrieves values of the matrix 
    get <- function() x
    
    ## sets the inverse of the matrix
    setInverse <- function(inverse) inv <<- inverse
    
    ## retrieves the inverse of the matrix
    getInverse <- function() inv
    
    list(set = set, get = get, setInverse = setInverse,
         getInverse = getInverse)

}


## This function computes the inverse of the special “matrix” returned by makeCacheMatrix 
##above. If the inverse has already been calculated( and the matrix has not changed,) 
##then the cachesolve should retrieve the inverse from the cache.


cacheSolve <- function(x, ...) {
    
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getInverse()
    
    # if the inverse has already been calculated
    if(!is.null(inv))
    {
        # gets it from the cache and skips the computation.
        message("getting cached data")
        return(inv)
    }
    
    # otherwise, calculates the inverse 
    matrix_data <- x$get()
    inv <- solve(matrix_data, ...)
    
    # sets the value of the inverse in the cache via the setinv function.
    x$setInverse(inv)
    
    ## returns the inverse
    inv
}
