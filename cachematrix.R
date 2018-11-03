## The first function makeCacheMatrix is a function
## that takes the inverse of a matrix

## Matrix inversion can be a costly computation
## so caching it can reduce the amount of iterations

## This function inverses the matrix object for caching

makeCacheMatrix <- function(x = matrix()) {
            inver <- NULL
            set <- function(y)  {
              x <<- y
              inver <<- NULL
            }
            get <- function() x
            setInverse <- function(inverse) inver <<- inverse
            getInverse <- function () inver
            list(set = set, get = get,
                 setInverse = setInverse,
                 getInverse = getInverse)
}


## This function will calculate the inverse of the matrix
## created from above.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inver <- x$getInverse()
        if(!is.null(inver)) {
                  message("getting cached data")
                  return(inver)
        }
        matri <- x$get()
        inver <- solve(matri, ...)
        x$setInverse(inver)
        inver
}
