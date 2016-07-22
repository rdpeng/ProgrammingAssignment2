## Rob Miller
## Programming Assignment 2
## July 22, 2016
##
##
## cacheMatrix.R contains two functions:
## makeCacheMatrix() that creates a special matrix, and
## cachSolve() that computes the inverse of the special matrix.

## makeCacheMatrix creates a special matrix
## 1. set the value of the matrix.
## 2. get the value of the matrix.
## 3. set the value of the inverse.
## 4. get teh value of the inverse.
## 
## the inverse is computed using solve()
makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) s <<- solve
        getinverse <- function() s
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve calculates the inverse of the matrix created 
## by makeCacheMatrix().
## checks to see if the cached matrix has been inverted.
## if there is no inverse, then cacheSolve 
## computes the inverse and returns the result as s.
cacheSolve <- function(x, ...) {
        s <- x$getinverse()
        if(!is.null(s)) {
                message("getting cached matrix")
                return(s)
        }
        matrix <- x$get()
        s <- solve(matrix, ...)
        x$setinverse(s)
        s
}
