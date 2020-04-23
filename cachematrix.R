## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## This pair of functions cash the inverse matrix, so that the inverse matrix is calculated in optimize time
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmatrix <- function(solve) m <<- solve
        getmatrix <- function() m
        list(set = set, get = get,
             setmatrix = setmatrix,
             getmatrix = getmatrix)
}

## Write a short comment describing this function
## Returns the inverse matrix. If the inverse has not been calculated
## before, the function calculates and chashes it, otherwise it returns
## the cashed value.
cachemean <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getmatrix()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setmatrix(m)
        m
}
