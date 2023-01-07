## Put comments here that give an overall description of what your
## functions do

## This is for the coursera data science: r programming certificate
# Week 3 Assignment; Github user: matthutchinson15
library(MASS)
makeCacheMatrix <- function(x = matrix()) {
 inv <- NULL    #initializ inverse as null
 set <- function(y) {
        x <<- y
        inv <<- NULL
 }
 get <- function() x  # function for matrix x
setinverse <- function(inverse) inv <<- inverse
getinverse <- function() inv # get inverse function
list(set = set, get = get, 
        setinverse = setinverse,
         getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {        #get cache data
        inv <- x$getinverse()
        if (!is.null(inv)) {        # check if inverse is null
                message("getting cached data!")
                return(inv)        #return inverse value
        }
        matrix_to_invert <- x$get()
        inv <- solve(matrix_to_invert, ...) #calc inverse value
        x$setinverse(inv)
        inv            ## Return a matrix that is the inverse of 'x'
}
my_Matrix <- makeCacheMatrix(matrix(1:4, 2, 2))
my_Matrix$get()

