##
## I set the input x as a matrix
## and then set the solved value "inver" as a null

makeCacheMatrix <- function(x = matrix()) {
  inver <- NULL
  set <- function(y) {
    x <<- y
    inver <<- NULL
  }

                get <- function() x
                setInverse <- function(inverse) inver <<- inverse
                getInverse <- function() inver
                list(set = set,
                     get = get,
                     setInverse = setInverse,
                     getInverse = getInverse)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
                inver <- x$getInverse()
                if (!is.null(inver)) {
                  message("getting inversed matrix")
                  return(inver)
                }
                matr <- x$get()
                inver <- solve(matr, ...)
                x$setInverse(inver)
                inver
}
