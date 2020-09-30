makeCacheMatrix <- function(x = matrix()) {
inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
        # use `<<-` to assign a value to an object in an environment
        # different from the current environment.
    }
    get <- function() x
    setinverse <- function(inverse) inverse <<- inverse
    getinverse <- function() inverse
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getinverse()
        if (!is.null(inverse)){
        return(inverse)
    }
    fac = x$get()
    inverse <- solve(fac, ...)
    x$setInv(inverse)
  inverse
}