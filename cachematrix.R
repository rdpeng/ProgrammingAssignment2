## Put comments here that give an overall description of what your
## functions do
## The functions calculate the inverse of a matrix, and the result is saved in cached.
## If the function is asked for the inverse matrix, it first check if it was calculated before and pass it
## or solve it and return it.

## Write a short comment describing this function
## makeCacheMatrix() takes a matrix as argument and it can be retrieved in get. Then the inverse matrix can be calculated
## with setinv, and can be retrieved with getinv. 

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(solve) m <<- solve
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}



## Write a short comment describing this function
## The inverse matrix is read in m, so it can be read by makeCacheMatrix to not calculate it again.
## If there is not a cached inverse matrix, it is calculated. The matrix to invert is retrieved in data
## and again the inverse is calculated and saved in m
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
}
