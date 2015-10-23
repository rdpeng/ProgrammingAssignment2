## x invertible matrix
## it returns a list containing the functions to set, get matrix, set the inverse and return inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInv <- function(inverse) inv <<- inverse
        getInv <- function() inv
        list(set = set,
             get = get,
             setInv = setInv,
             getInv = getInv)
}

## Here is the outout of the above function makeCacheMatrix
## returns the inverse of the function makeCacheMatrix
## This returns a matrix that is the inverse of x
cacheSolve <- function(x, ...) {
        inv <- x$getInv()
        if (!is.null(inv)) {
                message("retrieving cached data")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInv(inv)
        inv
}
