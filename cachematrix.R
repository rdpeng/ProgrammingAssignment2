## Put comments here that give an overall description of what your
## functions do
# Caching the Inverse of a Matrix:
## Caching the inverse of a matrix is more
## easy and fast than computing it repeatedly.
## A pair of functions are used to create a special object that 
## stores a matrix and then we cache its inverse.

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

}
##This function creates a special "matrix" object that can cache its inverse.

        makeCacheMatrix <- function(x = matrix()) {
           inv <- NULL
           set <- function(y) {
                x <<- y
                inv <<- NULL
           }
           get <- function() x
           setInverse <- function(inverse) inv <<- inverse
           getInverse <- function() inv
           list(set = set,
                get = get,
                setInverse = setInverse,
                getInverse = getInverse)
           }

           my_matrix <- makeCacheMatrix(matrix(1:4, 2, 2))
           my_matrix$get()

## Write a short comment describing this function
## This function computes the inverse of the special "matrix" created by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then it should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
}

           cacheSolve <- function(x, ...) {
                 ## Return a matrix that is the inverse of 'x'
                 inv <- x$getInverse()
                 if (!is.null(inv)) {
                          message("getting cached data")
                          return(inv)
                 }
                 mat <- x$get()
                 inv <- solve(mat, ...)
                 x$setInverse(inv)
                 inv
                 }

            my_matrix$getInverse()

            cacheSolve(my_matrix)
            my_matrix$getInverse()

            my_matrix$set(matrix(c(2, 2, 1, 4), 2, 2))
            my_matrix$get()

            my_matrix$getInverse()

            cacheSolve(my_matrix)
            my_matrix$getInverse()