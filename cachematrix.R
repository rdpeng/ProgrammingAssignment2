## Caching and Solve funcitios 


## Set and Get the value of the matrix; Set and Get the value of inverse of the matrix

makeCacheMatrix <- function (x = matrix()) {
        inverseMatrix <- NULL
        set <- function (y) {
                x <<- y
                inverseMatrix <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inverseMatrix <<- inverse
        getInverse <- function () inverseMatrix
        list(set=set, get=get, setInverse = setInverse, getInverse = getInverse)
}


## Return inverse of the matrix. Use methods of makeCacheMatrix

cacheSolve <- function (x, ...) {
        inverseMatrix <- x$getInverse()
        if (!is.null(inverseMatrix)) {
                message("getting cached data")
                return (inverseMatrix)
        }
        data <- x$get()
        inverseMatrix <- solve(data)
        x$setInverse(inverseMatrix)
        inverseMatrix
}
