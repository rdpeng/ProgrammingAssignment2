#The two functions makeCacheMatrix and cacheSolve are used to calculate
#and cache the inverse of a matrix, to avoid recalculating the inverse
#if it has already been calculated.

#makeCacheMatrix is a list of functions that will be used to 
#store a matrix x and cache the inverse of the matrix x. It returns 
#a cached list allowing the matrix and its inverse to be called again.

makeCacheMatrix <- function(x = matrix()) {
        I <- NULL
        set <- function(y) {
                x <<- y
                I <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) I <<- inverse
        getinverse <- function () I
        list(set = set, get = get, 
             setinverse = setinverse, 
             getinverse = getinverse)
}

#cacheSolve calculates the inverse of a matrix x, or retrives the inverse of x from the cache if it has already been solved for.

cacheSolve <- function(cache, ...) {
        # Return a matrix that is the inverse of 'x'
        I <- cache$getinverse()
        if(!is.null(I)) {
                message("getting cached data")
                return(I)
        }
        data <- cache$get()
        I <- solve(data, ...)
        cache$setinverse(I)
        I
}
