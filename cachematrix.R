## These two functions store a matrix in memory and then calculate, return, and
## store its inverse in memory if it has not yet been calculated. If it has been
## calculate then the cached inverse of the matrix is returned

## This function takes a matrix as its input and returns a list of functions that
## set the value of a matrix in memory, return that matrix, set the value of a
## matrix's inverse in memory and return the matrix's inverse

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
    setinverse = setinverse,
    getinverse = getinverse)

}


## This function takes a matrix as its input. It then checks to see if the value of
## the matrix's input has been set in memory; if this value exists, then it returns
## the message "getting cached data" and the matrix's inverse from the cache. If the
## matrix's inverse doesn't exist, the function calculates it and stored it in R's
## memory
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data)
    x$setinverse(i)
    i
}
