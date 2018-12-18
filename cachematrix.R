## This script contains two functions which create a special object
## that stores a matrix and caches its inverse

## The first function, makeCacheMatrix creates a special "matrix", '
##which is really a list containing a function to:
##set the matrix
##get the matrix
##set the inverse
##get the inverse

makeCacheMatrix <- function(x = matrix()) {
    m<-NULL
    set <- function(y) {
        x <<-y
        m <<- NULL
    }
    get <- function() x
    setinv <- function (solve) m <<- solve
    getinv <- function() m
    list (set=set, get=get, setinv=setinv, getinv=getinv)
}


## The following function calculates the inverse of a "matrix" created with 
## the above function. However, it first checks to see if the inverse has 
##already been calculated. If so, it gets the inverse from the cache and 
##skips the computation. Otherwise, it calculates the inverse of the matrix 
##and sets the inverse in the cache via the setinv function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getinv()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinv(m)
    m
}
