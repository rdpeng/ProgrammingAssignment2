## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
            set <- function(y) {
                    x <<- y
                    inv <<- NULL
            }
            get <- function() x
            setsolve <- function(solve) inv <<- solve
            getsolve <- function() inv
            list(set = set, get = get,
                 setsolve = setsolve,
                 getsolve = getsolve)
}

## Return a matrix that is the inverse of 'x'
## This function computes the inverse of the special "matrix" returned by `makeCacheMatrix` above.
## If the inverse has already been calculated (and the matrix has not changed), then`cacheSolve`
## should retrieve the inverse from the cache. cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getsolve()
            if(!is.null(inv)) {
                    message("getting cached data")
                    return(inv)
            }
            data <- x$get()
            inv <- solve(data, ...)
            x$setsolve(inv)
            inv        
        
}
