

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        setMatrix <- function(y) {
                x <<- y
                print(x)
                inverse <<- NULL
        }
        getMatrix <- function() x
        setInverse <- function(solve) inverse <<- solve
        getInverse <- function() inverse
        list(setMatrix = setMatrix, getMatrix = getMatrix,
             setInverse = setInverse,
             getInverse = getInverse,x=x)
        
        
}



cacheSolve <- function(x, ...) {
        mat <- x$getInverse()
        if(!is.null(mat)) {
                message("getting cached data")
                return(mat)
        }
        data <- x$getMatrix()
        mat <- solve(data, ...)
        x$setMatrix(mat)
        mat
}
