##The two functions makeCacheMatrix and cacheSolve are used to calculate
##and cache the inverse of a metrix, to avoid recalculating the inverse
##if it has already been calculated.

## Write a short comment describing this function

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


##cacheSolve calculates the inverse of a matrix x, or retrives the inverse

cacheSolve <- function(cache, ...) {
        ## Return a matrix that is the inverse of 'x'
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