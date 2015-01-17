##  Caching the Inverse of a Matrix

## This first function, makeCacheMatrix creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        ## set() sets the value of the matrix 
        set <- function(y) {
                x <<- y
                i <<- NULL
        }                
        ## get() gets the value of the matrix
        get <- function() x
        
        ## setinverse() sets the value of the inverse of a matrix
        setinverse <- function(inverse) i <<- inverse
        
        ## getinverse() gets the value of the inverse of a matrix
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## This second function, cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- inverse(data, ...)
        x$setinverse(i)
        i
}