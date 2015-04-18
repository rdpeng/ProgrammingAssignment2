## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
                inverse <- NULL
                set <- function(y) {
                                x <<- y
                                inverse <<- NULL
                }
                get <- function() x
                setinverse <- function(Inverse) inverse <<- Inverse
                getinverse <- function() inverse
                list(set = set, get = get,
                        setinverse = setinverse,
                        getinverse = getinverse)
}



## Write a short comment describing this function
# cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
# Computing the inverse of a square matrix can be done with the solve function in R. For example, if X is a square invertible matrix, then solve(X) returns its inverse.
# For this assignment, assume that the matrix supplied is always invertible.

cacheSolve <- function(x, ...) {
                ## Return a matrix that is the inverse of 'x'
                inverse <- x$getinverse()
                        if(!is.null(inverse)) {
                        message("getting matrix stored in memory :)")
                        return(inverse)
                        }
                data <- x$get()
                inverse <- solve(data, ...)
                x$setinverse(inverse)
                inverse
}


######TEST ZONE######
#https://class.coursera.org/rprog-013/forum/thread?thread_id=125
#Unit tests (with expected output) for Programming Assignment 2
#
#assignment operator: <- (in current environment / frame)
#superassignment operator: <<-  (in parent environment / frame)
#
### Example
#
#>    source("cachematrix.R")
#
#>    amatrix = makeCacheMatrix(matrix(c(1,2,3,4), nrow=2, ncol=2))
#>    amatrix$get()         # Returns original matrix
#[,1] [,2]
#[1,]    1    3
#[2,]    2    4
#
#>   cacheSolve(amatrix)   # Computes, caches, and returns    matrix inverse
#[,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5
#
#>  amatrix$getinverse()  # Returns matrix inverse
#[,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5
#
#>  cacheSolve(amatrix)   # Returns cached matrix inverse using previously computed matrix inverse
#getting cached data
#[,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5
#
#>    amatrix$set(matrix(c(0,5,99,66), nrow=2, ncol=2)) # Modify existing matrix
#>    cacheSolve(amatrix)   # Computes, caches, and returns new matrix inverse
#[,1] [,2]
#[1,] -0.13333333  0.2
#[2,]  0.01010101  0.0
#
#>    amatrix$get()         # Returns matrix
#[,1] [,2]
#[1,]    0   99
#[2,]    5   66
#
#>    amatrix$getinverse()  # Returns matrix inverse
#[,1] [,2]
#[1,] -0.13333333  0.2
#[2,]  0.01010101  0.0