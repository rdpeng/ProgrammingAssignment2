## The first function creates matrix and cache it while the second one 
## checks for the inverse of the matrix in cache,if present it is returned else
##inverse is computed

## This function creates matrix and cache it's inverse

makeCacheMatrix <- function(x = matrix()) {
        inV <- NULL
        set <- function(y) {
                x <<- y
                inV <<- NULL
        }
        get <- function() x
        setinverse <- function(matInv) inV <<- matInv
        getinverse <- function() inV
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)



}


## This function computes the inverse of the matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inV <- x$getinverse()
        if(!is.null(inV)) {
                message("getting cached data")
                return(inV)
        }
        data <- x$get()
        inV <- solve(data, ...)
        x$setinverse(inV)
        inV

}
