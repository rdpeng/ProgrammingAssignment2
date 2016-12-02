## Put comments here that give an overall description of what your
## functions do

## This function creates a cache to calculate the inverse of a matrix.
## There's a place holder to store, calculate, retrieve,
## and make available the function.

makeCacheMatrix <- function(x = matrix()) {

## Initialize with an empty vector to hold cache
        inv = NULL
## Store function
        set = function (y) {
              x <<- y
              inv <<- NULL
        }
## Retrieve function
        get = function() x
## Store inverted matrix
        setinv = function(inverse) inv <<- inverse
## Retrieve inverted matrix
        getinv = function() inv
## Make functions available
        list(set = set, get = get,
        setinv = setinv, getinv = getinv)
}


## This function takes the inverse matrix, first checks to see
## if matrix is available in cache. Otherwise, it calculates
## and returns the inverse.

cacheSolve <- function(x, ...) {

        ## Return a matrix that is the inverse of 'x'
        inv = x$getinv()
        ## Check if inverse exists
        if(!is.null(inv)) {
              message("getting cached data")
              return(inv)
        }

        ## Otherwise, calculate and return inverse of matrix
        data = x$get()
        inv = solve(data, ...)
        x$setinv(inv)
        return(inv)

}
