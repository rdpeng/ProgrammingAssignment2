## Put comments here that give an overall description of what your
## functions do
## This is a programming assignment-2 from Coursera R course and aim of this is to create two functions that caches the inverse of a matrix.

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

}

## My comments of above function are as fololws: 
## makeCacheMatrix takes one argument, x, which is a matrix
## This function creates a special "matrix" object and store it.
## makeCacheMatrix takes one argument, x, which is a matrix
## This function creates a special "matrix" object and store it.
inv <- NULL # define vector to store the caching inversed matrix
set <- function(y){ # define set function that can define matrix x
x <<- y
inv <- NULL # make sure that this vector is cleared when function is used again
}
get <- function() x # define "get" function that gets the contents of matrix x
# look for the value of x outside the function environment
set_inv <- function(inversion) inv <<- inversion 
# define matrix "set_inv" that calculates the inversion of matrix x
# look for the value outside the function environment
get_inv <- function() inv #define matrix "get_inv" that gets the inversion of matrix x
# look for the value outside the function environment
# organize the set/get functions into a list, and give the variables headers
# output of this function
list(set=set, get=get, set_inv=set_inv, get_inv = get_inv) 
 }

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
}
# My short comments are as follows for cacheSolve <- function(x, ...) {
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.
	
cacheSolve <- function(x, ...) {  ## Return a matrix that is the inverse of 'x'
inv <- x$get_inv()  
if (!is.null(inv)) {
message("get cached inversion data")
return(inv)
}
data <- x$get()
inv <- solve(data)
x$set_inv(inv)
inv
  }


        
