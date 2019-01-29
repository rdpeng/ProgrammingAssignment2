## Assignment 2
## Ramiro Zermeño Díaz

## makeCacheMatrix creates a special "matrix", which is really
## list containing a function to:

## 1.- Set the value of the matrix
## 2.- Get the value of the matrix
## 3.- Set the value of the inverse matrix
## 4.- Get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInverse <- function(solve) m <<- solve
        getInverse <- function() m
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## cacheSolve calculates the inverse matrix of the special "matrix"
## created above. However, it first checks to see if the inverse
## has already been calculated. If so, it gets the inverse matrix
## from the cache and skips the computation. Otherwise, it calculates
## the inverse matrix of the data and sets the value of the inverse
## in the cache via the setInverse function.

cacheSolve <- function(x, ...) {
        invmat <- x$getInverse()
        if(!is.null(invmat)) {
                message("getting cached data")
                return(invmat)
        }
        data <- x$get()
        invmat <- solve(data, ...)
        x$setInverse(invmat)
        invmat
}