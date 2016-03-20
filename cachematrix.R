##makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
##cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already 
##been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

##This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {    ##passing a matrixto the function and assigning it to makeCacheMatrix
        invs <- NULL
        set <- function(y) {                   ##setting value of x in parent environment which may be different in another environment
                x <<- y
                invs <<- NULL
        }
        get <- function() x                                ## returns value of matrix argument
        setInverse <- function(inverse) invs <<- inverse   ## assigns value of invs in parent environment
        getInverse <- function() invs                      ## gets the value of invs where called
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)

}


##cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already 
##been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invs <- x$getInverse()
        if (!is.null(invs)) {
                message("getting cached data")
                return(invs)
        }
        mat <- x$get()
        invs <- solve(mat, ...)      ##Solve function to get inverse of the matrix
        x$setInverse(invs)
        invs
}
