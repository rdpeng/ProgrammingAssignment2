## Put comments here that give an overall description of what your
## functions do

## The makeCacheMatrix function follows the template for the caching
## of the vector mean example provided in the ReadMe file.  Namely,
## it creates a list of four functions that set the matrix, gets the
## matrix, sets the inverse, and gets the inverse.

## The set function sets and caches a new matrix.  When a new matrix is cached,
## inv is set to null (as we don't want to use a previously cached inverse on a new matrix).
## The get function returns the currently set matrix.  The setinv function takes the currently
## set matrix and caches its inverse.  The getinv function returns the currently cached inverse.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## Write a short comment describing this function

## This function begins by calling the getinv() function from the makeCacheMatrix function.
## If that function returns a non-null value, than the inverse was previously cached
## and we return that cached value.  If the getinv() function returns null, then the
## inverse has not previously been calculated.  We get the currently set matrix, and use
## the solve function to set its inverse using the setinv() function.  Then we return the
## newly calculated and cached inverse matrix.

cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    matrix <- x$get()
    inv <- solve(matrix, ...)
    x$setinv(inv)
    inv
}
