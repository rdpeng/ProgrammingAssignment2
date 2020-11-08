##Both functions below help in caching the inverse of a matrix
##
##The function below helps to caching the inverse of a matrix by creating
##a special matrix that can cahe it's inverse


makeCacheMatrix <- function(m = matrix()) {
        inv <- NULL
        set <- function(x) {
                m <<- x
                inv <<- NULL
        }
        get <- function() m
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)

}


##This function inverses a matrix through retrieving the inverse from the cache

cacheSolve <- function(x, ...) {
         i <- x$getInverse()

        if( !is.null(i) ) {
            message("getting cached data")
            return(i)
    }
        data <- x$get()
        i <- solve(data, ...)
        x$setInverse(i)
        i
}
