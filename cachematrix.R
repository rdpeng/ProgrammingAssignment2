## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        ## valuable "inv" is undefined, so set as "NULL"
        set <- function(y)
        ## set function and belows as like example of "makevector"
        x <<- y
        inv <<- NULL
        ## define "x" , "inv" with different environment with "<<-" 
        get <- function() x
        setinv <- function(solve) inv <<- solve(x)
        ## solve(x) function inverse x= matrix()
        getinv <- function() inv
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}
## this function computes the inverse of the spceial "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been calculated 
##(and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        ## if "getinv" value is not "NULL", print "getting cached data"
        data <- x$get()
        inv <- solve(data, ... )
        ## "solve" function inverse the matrix(x)
        x$setinv(inv)
        inv
}
