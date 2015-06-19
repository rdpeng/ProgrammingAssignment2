## This first function computes a inverse of a matrix if there exist
makeCacheMatrix <- function(x = matrix()) {
    ## Initialization
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    ## setting the inverse function
    setinverse <- function(solve) m <<- solve
    ## obtaining the inverse
    getinverse <- function() m
    ## resulting list
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
    
}

## Take a matrix and look for its inverse
## If there is not cached, it is computed
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        ## Showing the inverse cached
        return(m)
    }
    ## Getting the inverse 
    data <- x$get()
    m <- solve(data, ...)
    ## Saving the new inverse 
    x$setinverse(m)
    m
}