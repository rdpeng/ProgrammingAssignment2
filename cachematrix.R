## [function to make cache matrix]

makeCacheMatrix <- function(x = matrix()) {
nv <- NULL
        set <- function(y) {
                x <<- y
                nv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) nv <<- inverse
        getinverse <- function() nv
        list(set = set,
             get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
nv <- x$getInverse()
        if (!is.null(nv)) {
                message("getting cached data")
                return(nv)
        }
        mat <- x$get()
        nv <- solve(mat, ...)
        x$setinverse(nv)
        nv
}
