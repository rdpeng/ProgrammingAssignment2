## Cache the inverse of a matrix

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        invResult <- NULL
        set <- function(y) {
              x <<- y
              invResult <<- NULL
        }
        get <- function() {
              x
        }
        setInv <- function(inverse) {inv <<- inverse}
        getInv <- function() {
              invResult
        }
        list(set=set, get=get, setInv=setInv, getInv=getInv)
}

## This function computes the inverse of the special "matrix" returned by the function above

cacheSolve <- function(x, ...) {
        invResult <- x$getInverse()
        if(!is.null(invResult)) {
              message("Getting cached data")
              return(invResult)
        }
        mat <- x$get()
        invResult <- solve(mat, ...)
        x$setInverse(invResult)
        invResult
}
