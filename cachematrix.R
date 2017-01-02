## Put comments here that give an overall description of what your
## functions do

## this functions were written in order to :
##  1. verify if an inverse matrix has been calculated
##  2. and if not, 
##     2.1  calculate and store its value to be compared later (another execution)


# how to test it :
#  1. clean all enviroment variables
#     remove(list=ls())
#
#  2. create 3 matrices (2 equals, 1 different)
#     matrix_1 <- matrix(c(2, 4, 3, 1), nrow=2, ncol=2)
#     matrix_2 <- matrix(c(2, 4, 3, 1), nrow=2, ncol=2)   (same as matrix 1)
#     matrix_3 <- matrix(c(5, 6, 7, 8), nrow=2, ncol=2)
#
#  3. run source("cachematrix.R")
#
#  4. run makeCacheMatrix(matrix_1)
#     system says "did not found cached matrix" and calculates inverse matrix
#
#  5. run makeCacheMatrix(matrix_2) (same as matrix_1)
#     system says "found cached matrix" and returns it
#
#  6. run makeCacheMatrix(matrix_3) 
#     system says "did not found cached matrix" and calculates inverse matrix



makeCacheMatrix <- function(x = matrix()) {
  ## important : "For this assignment, assume that the matrix supplied is always invertible"
    inverse_matrix <- cacheSolve(x)
    cat(paste("inverse matrix = ", print(inverse_matrix)))
}


## Write a short comment describing this function
cacheSolve <- function(y = matrix()) {
        ## Return a matrix that is the inverse of 'x'
        ## important : "For this assignment, assume that the matrix supplied is always invertible"
        if (!exists("previous_matrix")) {
          previous_matrix <<- matrix()
        } 
        if (identical(y, previous_matrix)) {
          cat(paste("found cached matrix", "\n"))
          inverse_matrix
        } else {
          cat(paste("did not find cached matrix or matrix has changed", "\n"))
          previous_matrix <<- y
          inverse_matrix <<- solve(y)
          inverse_matrix
        }
}





