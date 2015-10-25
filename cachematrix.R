# makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    invMatrix <- NULL
    set <- function(y) {
        x <<- y
        invMatrix <<- NULL
    }
    get <- function() x
    setInvMatrix <- function(inverse) invMatrix <<- inverse
    setInvMatrix <- function() invMatrix
    list(set=set, get=get, setInvMatrix=setInvMatrix, setInvMatrix=setInvMatrix)
}


# cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
# If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

# This function assumes that the matrix is always invertible.
cacheSolve <- function(x, ...) {
    invMatrix <- x$setInvMatrix()
    if(!is.null(invMatrix)) {
        message("getting cached data.")
        return(invMatrix)
    }
    data <- x$get()
    invMatrix <- solve(data)
    x$setInvMatrix(invMatrix)
    invMatrix
}
