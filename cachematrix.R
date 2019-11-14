## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
# #cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve 
## the inverse from the cache.

makeCacheMatrix <- function(x = matrix()) {
        
        inv <= NULL
        set <- function(matrix) {
                
                m <<- matrix
                inv <<- NULL
                }
        get <- function() {
                m
                }
        
        set_inverse <- function(inverse) {
        i <<- inverse
                }
        getInverse <- function() {
      
        i
                }
        list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
        
}


## This function caches the calculation of the inverse of a matrix

cacheSolve <- function(x, ...) {

    m <- x$getInverse()
        
    if( !is.null(m) ) {
            message("getting cached data")
            return(m)
    }

    data <- x$get()

    m <- solve(data) %*% data

    x$setInverse(m)

    m
}
