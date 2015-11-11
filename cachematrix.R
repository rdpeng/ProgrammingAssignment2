## Example of caching the results of intensive calculations. Once calculated,
## the result can be reused from cache as long as teh data is not changed.
## Next step would be to encapculate the calculation in the getinv function, 
## which would make the caller agnostic to teh existance of teh cache.

## Returns a list of functions that can be used to calculate the 
## inverse of a matrix and cache the results for subsequent uses

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(m){
         x <<- m
         inv <<- NULL
    } 
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## Uses the inverse matrix from cache or calculates it 
## if it doesn't exist.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        m <- x$get()
        inv <- solve(m, ...)
        x$setinv(inv)
        inv
}

