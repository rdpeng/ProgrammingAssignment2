makeCacheMatrix <- function(x = matrix()) {
        ## set inv to null so it can be stored in the cache as that if it doesn't exist
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        ## if it isn't null then do the following...
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        ##setting x to the inverse of inv
        x$setInverse(inv)
        inv
}
