makeVector <- function(x = numeric()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmean <- function(mean) m <<- mean
        getmean <- function() m
        list(set = set, get = get,
             setmean = setmean,
             getmean = getmean)
}

cachemean <- function(x, ...) {
        m <- x$getmean()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- mean(data, ...)
        x$setmean(m)
        m
}

## Functions that can cache the inverse of a matrix.
## Because Matrix inversion is often a costly computation, caching the inverse  
## of a matrix rather than computing it repeatedly can save time and money.
## The following functions creates a special object which stores a matrix
## and caches its inverse.

## The first function creates the special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL             #initial inverse as NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x     #function to get matrix x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)   #function to get inverse of the matrix
}


## This function computes the inverse of the special "matrix" created by 
## makeCacheMatrix above. Since the inverse was calculated, and the matrix is
## still the same, then it should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        hold <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)
        inv
}












