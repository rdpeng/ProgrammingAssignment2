## Put comments here that give an overall description of what your
## functions do
## These functions, used in combination, take a matrix as an argument and return the inverse of that matrix
## BUT only *calculating* the inverse if a cached value of the inverse is not already available. If it is,
## the functions return this cached value of the inverse. If the inverse is calculated, it is then cached.

## Write a short comment describing this function
## makeCacheMatrix is a function which takes a matrix as argument and returns a list of four functions:
## 'set' will set the value of the matrix;
## 'get' will get the value of the matrix;
## 'set_inv' will set the value of the inverse of the matrix to be cached;
## 'get_inv' will get the cached value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        set_inv <- function(solve) inv <<- solve
        get_inv <- function() inv
        list(set = set, get = get,
             set_inv = set_inv,
             get_inv = get_inv)
        
}

## Write a short comment describing this function
## cacheSolve is a function that takes as argument the list created by makeCacheMatrix, checks if
## a chached value of the matrix is available and returns this value if it is. If not, the function calculates
## the inverse of the matrix and returns this, and it assigns this calculated value to the chached value.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$get_inv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$set_inv(inv)
        inv
}
