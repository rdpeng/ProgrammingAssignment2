## The makeCacheMatrix and cacheSolve functions set the values of Matrices and their inverse in the cache

#The makeCacheMatrix function caches the value of a matrix, retrieves that value, caches the value of its inverse and retreives that value
#It returns a list of the functions, to set and get the values for the matrix and its inverse.


makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


#The CacheSolve function checks the cache for the inverse of the input matrix, and if there is none it computes the value of the matrix's inverse.
#It returns the cached inverse, or the computed inverse of the matrix.

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
