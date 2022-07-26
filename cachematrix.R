## These functions are working together, to first ensure if the inverse of a matrix i was already calculated. 
## If so, the calculated inverse is given back. If not, the function computes the inverse by using the "solve" function

## provides the functions for cacheSolve

makeCacheMatrix <- function(i = matrix()) {
 inv <- NULL
        set <- function(y) {
                i <<- y
                inv <<- NULL
        }
        get <- function() i
        setinv <- function(solve) inv <<- solve
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## checks if the inverse was calculated, if not, calculates the inverse

cacheSolve <- function(i, ...) {
        inv <- i$getinv()
        if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
        }
        data <- i$get()
        inv <- solve(data)
        i$setinv(inv)
        inv
}
