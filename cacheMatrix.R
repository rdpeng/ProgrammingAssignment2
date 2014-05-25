makeCacheMatrix <- function(x = matrix()) {
        invMat <- NULL
        set <- function(y) {
                x <<- y
                invMat <<- NULL
        }
        get <- function() x
        setinverse <- function(inverseMat) invMat <<- inverseMat
        getinverse <- function() invMat
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

cacheSolve <- function(x, ...) {
        invMat <- x$getinverse()
        if(!is.null(invMat)) {
                message("getting cached inverse matrix")
                return(invMat)
        }
        data <- x$get()
        invMat <- solve(x, ...)
        x$setinverse(invMat)
        invMat
}
