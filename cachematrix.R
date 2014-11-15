## Computing the inverse of the matrix with its caching after first calc  

## makeCacheMatrix is basically setting up the matrix by makeCacheMatrix or $setmx (mx-is short for matrix) 
## and stores inversed matrix in getinvmx(inv - is short for inverted)

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        setmx <- function(y) {
                x <<- y
                m <<- NULL
        }
        getmx <- function() x
        setinvmx <- function(mean) m <<- mean
        getinvmx <- function() m
        list(setmx = setmx, getmx = getmx,
             setinvmx = setinvmx,
             getinvmx = getinvmx)
}

## retrieving $getinvmx from previous function, do the math solve and send back $getinvmx in case retrieved data is NULL
## or just returning previously calculated $getinvmx


cacheSolve <- function(x, ...) {
        m <- x$getinvmx()
        if(!is.null(m)) {
                message("getting cached inversed matrix")
                return(m)
        }
        data <- x$getmx()
        m <- solve(data, ...)
        x$setinvmx(m)
        m  ## Return a matrix that is the inverse of 'x'
}
