## As part of R programming assignment 2 we're asked to 
## write pair of functions that cache inverse of a matrix.

## makeCacheMatrix function will create a special "matrix" 
## object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        ## initialize
        m <- NULL
        
        ## set matrix function
        set <- function (matrix) {
                x <<- matrix
                m <<- NULL
        }
        
        ## get matrix function
        get <- function() {
                x
        }
        
        ## set inverse matrix function
        setInverse <- function(inverse) {
                m <<- inverse
        }
        
        ## get inverse matrix function
        getInverse <- function () {
                m
        }
        
        ## return list of functions
        list (set = set,
              get = get,
              setInverse = setInverse
              getInverse = getInverse)
}


## cacheSolve function will compute the inverse of the special "matrix"
## returned by makeCacheMatrix above. If the inverse has already been 
## calculated (and the matrix has not changed), then the CacheSolve
## function should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m < x$getInverse()
        
        ## Retrieve inverse from cache
        if (!is.null(m)) {
                message("Inverse already calculated. get cached data")
                return(m)
        }
        
        
        ## get matrix
        data <- x$get()
        
        ## calculate inverse matrix
        m <- solve(data) %*% data
        
        ## set inverse matrix
        x$setInverse(m)
        
        ## return matrix
        m
}
