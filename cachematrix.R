# Two functions to cache the inverse of a matrix

# This function creates a special matrix object that can cache its inverse.
makeCacheMatrix <- function(x = matrix() ) {
    
    # initialize the inverse
    i <- NULL
    
    # set the matrix
    set <- function(matrix) {
        m <<- matrix
        i <<- NULL
    }
    
    # get the matrix
    get <- function() m
    
    # set the inverse of the matrix
    setInverse <- function(inverse) i <<- inverse
    
    # get the inverse of the matrix
    getInverse <- function() inv
    
    # return the list of methods
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


# This function computes the inverse of the special matrix returned by 
# makeCacheMatrix function above. If the inverse has already been calculated 
# (and the matrix has not changed), then the cachesolve should retrieve the 
# inverse from the cache.

cacheSolve <- function(x, ...) {
    # Return a matrix that is the inverse of 'x'
    m <- x$getInverse()
    
    # if already calculated, retrieve inverse matrix from cache
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    
    # get the matrix from the object
    data <- x$get()
    
    # calculates the inverse matrix of data using matrix multiplication
    m <- solve(data) %*% data
    
    # set the inverse to the object
    x$setInverse(m)
    
    # return the matrix
    m
}
