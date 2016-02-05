## Here we provide two functions, which, together, make a 'cached' 
## version of the 'solve' function (which calculates the inverse of a 
## matrix). Matrix inversion can take a long time for big matrices,
## and having it cached could save time in certain cases.

## The function makeCacheMatrix creates a list of functions that allows to
## 'set' - set the value of the matrix
## 'get' - get the value of the matrix
## 'setinv' - set the inverse of the matrix
## 'getinv' - get the inverse of the matrix
##
## Example usage:
## x <- makeCacheMatrix(matrix(c(4,3,3,2), nrow=2, ncol=2)) loads a
## 2x2 matrix into x
## 
## x$get() shows the contents of the matrix stored in x
## x$set(y) sets y as the value for the matrix in x
## x$setinv() used by cacheSolve (sets inverse)
## x$getinv() used by cacheSolve (gets inverse)

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinv <- function(solve) i <<- solve
        getinv <- function() i
        list(set = set, get = get,
             setinv = setinv, getinv = getinv)
}


## The function cacheSolve(x) returns the inverse of the matrix x
## If the inverse has already been calculated before, it will just return
## the pre-calculated (cached) version. Otherwise it will calculate it
## on the spot.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinv()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinv(i)
        i
}
