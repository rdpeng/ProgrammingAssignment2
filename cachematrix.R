## Because Matrix inversion is sually a cosly computation, we use two funtions that help us caching the inverse matrix instead of computing it repeatedly.

## This function creates a special "matrix" object that can cache its inverse




makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y){
        x <<- y
        m <<- NULL
    }
    
    get <- function()x
    setmatrix <- function(solve) m <<- solve
    getmatrix <- function() m
    list(set = set, get = get, setmatrix = setmatrix, getmatrix = getmatrix)


}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getmatrix()
        if(!is.null(m)){
            message("getting cached data")
            retun(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setmatrix(m)
        m
        
}


