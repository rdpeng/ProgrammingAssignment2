
## Creating a matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {

    i <- NULL
    
  ## set function for the matrix
    set <- function( matrix ) {
            m <<- matrix
            i <<- NULL
    }
    
  ## get function for the matrix
    get <- function() {
    	m
    } 
    
   ##this function sets the inverse of the matrix 
    setInverse <- function(inverse) {
        i <<- inverse
    }
   ## this method returns the inverse of the matrix
    getInverse <- function() {
        i
    }
    
    ##this will return a list of the functions
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## This computes the inverse of the special matrix returned by "makeCacheMatrix"
## function above. If the inverse has already been calculated (and the matrix hasn't
## changed), then the "cachesolve" should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getInverse()
    
    ##return the inverse if it is already set
    if( !is.null(m) ) {
            message("getting cached data")
            return(m)
    }
    ##retrieve the matrix from the object
    data <- x$get()
    ##make use of matrix multiplication to inverse the matrix
    m <- solve(data) %*% data
    ##set the inverse of the matrix
    x$setInverse(m)
    
    ##return it
    m
}
