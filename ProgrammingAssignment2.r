## Put comments here that give an overall description of what your
## functions do
##makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

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

##cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
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
