## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        nv <- x$getinverse()
        if (!is.null(nv)) {
                message("getting cached data")
                return(nv)
        }
        mat <- x$get()
        nv <- solve(mat, ...)
        x$setinverse(nv)
        nv
}
