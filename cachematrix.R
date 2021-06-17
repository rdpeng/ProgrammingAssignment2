## Write a short comment describing this function
# The function makeCacheMatrix generates a matrix which is able to be retrieved from the cache or stored in the cache

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL 
        set <- function(y) { 
                x <<- y 
                inv <<- NULL  
        }
        get <- function() {x} 
        setInverse <- function(inverse) {inv <<- inverse}  
        getInverse <- function() {inv} 
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}
## Write a short comment describing this function
# cacheSolve is a function that returns a matrix from makeCacheMatrix to compute the inverse of x if not found in the cache. 
# Otherwise it is obtained from the cache displaying a message "getting cached data" to provide the inverse of the matrix called.

cacheSolve <- function(x, ...) {
        inv <- x$getInverse() 
        if(!is.null(inv)) { 
                message("getting cached data") 
                return(inv) 
        }
        data <- x$get() 
        inv <- solve(data, ...) 
        x$setInverse(inv) 
        inv
}
