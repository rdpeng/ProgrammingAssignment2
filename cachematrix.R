## [function to make cache matrix]

makeCacheMatrix <- function(x = matrix()) {
# to return a list that containing functions
        nv <- NULL
        set <- function(y) {
# to assign a value of an onject in an different environment
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
        ## to return a inverse of the matrix input to makecachematrix()
nv <- x$getinverse()
        if (!is.null(nv)) {
#to get it from cache and to skip this computation
                message("getting cached data")
                return(nv)
        }
# to calculate the inverse otherwise
        mat <- x$get()
        nv <- solve(mat, ...)
# to set the value of the inverse in the cache        
        x$setinverse(nv)
        nv
}
