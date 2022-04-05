##  A pair of functions that cache the inverse of a matrix.


## Create a list of function that set and cache the original and 
## inverse matrix (if any)

makeCacheMatrix <- function(x = matrix()) {
                inv <- NULL
                set <- function(y){
                        x <<- y
                        inv <<- NULL 
                }
                get <- function() x
                setinv <- function(set_inv) inv <<- set_inv
                getinv <- function() inv
                list(set = set, get = get,
                setinv = setinv,
                getinv = getinv)
}


## Compute the inverse of the matrix if it has not been calculated or set.
## For the latter case, cached inverse will be retrieved and returned

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        matr <- x$get()
        inv <- solve(matr, ...)
        x$setinv(inv)
}
