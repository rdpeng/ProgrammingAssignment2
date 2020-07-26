## Put comments here that give an overall description of what your
## functions do

## For this assignment, the two functions 'makeCacheMatrix' and 'cacheSolve' 
## are used to cache the inverse of matrix, which would otherwise be a very timely
## process that requires repetetive computing.

## Write a short comment describing this function

## makeCacheMatrix is a function that generates a special "matrix" that is 
## able to cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## Write a short comment describing this function

## cacheSolve is a function that calculates the inverse of the specia "matrix" that 
## 'makeCacheMatrix'created. It retrieves the inverse from the cache if 
## the inverse has already been determined and if the matrix remains unchanged.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setInverse(inv)
        inv
}
