## Description: The fucntion cacheSolve should compute and return 
# the inverse of the makeCacheMatrix. In case the inverse was
# already calculated, cacheSolve will retrieve the inverse 
# from the cache.

## I need to write a function that will create a special matrix object 
# and return and cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        y <- NULL
        set <- function(y) {
            x <<- y
            m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## In this function I need to compute the inverse of the 
# special matrix object I created above.

cacheSolve <- function(x, ...) {
    
    m <- x$getinverse()
    if(!is.null(m)) {
        if(x$set() == s$get())
        return(m)
    }
    y <- x$get()
    m <- solve(y, ...)
    x$setinverse(m)
    m
        ## Return a matrix that is the inverse of 'x'
}



# EXAMPLE

mat <- matrix(c(7,9,3,2), 2, 2)
mat

cachedmat <- makeCacheMatrix(mat)
cachedmat

cacheSolve(cachedmat)