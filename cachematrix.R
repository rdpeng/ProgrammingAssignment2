## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
     inv <- NULL
     set <- function(y) {
             x <<- y
             inv <<- NULL
}
        get <- function() x
        setinv <- function(solve) inv <<- solve        
        getinv <- function() inv
        list(sest=set, get=get,
             setinv=setnv,getinv=getinv)
## Write a short comment describing this function
}
cacheSolve <- function(x, ...) {
        mat <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data,...)
        x$setinv(inv)
        inv
}
