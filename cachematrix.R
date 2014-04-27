##      makeCacheMatrix: This function creates a special "matrix" object that
##                       can cache its inverse.
##      set: set the values of the matrix
##      get: get the values of the matrix
##      setinv: set the inverse of a square matrix
##      getinv: get the inverse of a square matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y){
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(solve) inv <<- solve
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)

}

##      cacheSolve: computes the inverse of the special "matrix" returned by
##                  makeCacheMatrix above. If the inverse has already been
##                  calculated (and the matrix has not changed), then cacheSolve
##                  should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)){
                message("getting cache data")
                return(inv)
        }
        
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}
