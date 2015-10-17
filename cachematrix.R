## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## makeCacheMatrix create a special object that stores a matrix and 
## cache its inverse. The special object is a list containing a function to
## set the value of the matrix
## get the value of the matrix
## set the value of the inversed matrix
## get the value of the inversed matrix

makeCacheMatrix <- function(x = matrix()) {
       
        xinv <- NULL
        
        set_matrix <- function(y) {
                x <<- y
                xinv <<- NULL
        }
        
        get_matrix <- function() x
        
        set_inv_matrix <- function(inversed_mx) xinv <<- inversed_mx
        
        get_inv_matrix <- function() xinv
        
        list(set_matrix =set_matrix, get_matrix = get_matrix,
             set_inv_matrix = set_inv_matrix,
             get_inv_matrix = get_inv_matrix)

}


## Write a short comment describing this function

## The following function calculates the inverse of a matrix created 
## with the above function. However, it first checks to see if the inverse 
## has already been calculated. If so, it gets the inverse from the cache and 
## skips the computation. Otherwise, it calculates the inverse of the matrix
## and sets the inverse of the matrix in the cache via the set_inv_matrix function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        xinv <- x$get_inv_matrix()
        if(!is.null(xinv)) {
                message("getting cached data")
                return(xinv)
        }
        data <- x$get_matrix()
        xinv <- solve(data, ...)
        x$set_inv_matrix(xinv)
        xinv
}
