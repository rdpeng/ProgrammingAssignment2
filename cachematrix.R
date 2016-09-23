## Title: " Catching the Inverse of a Matrix"
## Program: "R - Programming Assignment 2"
Date: " September 9, 2016"
Author: Marida Gingras


## Natrix inversion is a costly computation. 
## There may be some benefit to caching the inverse of a matrix rather than compute it repeatly. 
## Functions below are a pair of functions cache the inverse of a matrix.

makeCacheMatrix <- function(x = matrix()) {
inv <- NULL
set <- function(y){
x <<-y
inv <<-NULL
}
get <- function()x
setInverse <- function(inverse) inv <<- inverse
getInverse <- function() inv
list (set=set, 
get=get, 
setInverse=setInverse, 
getInverse=getInverse)
}



## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if(!is.null(inv)){
        message("getting cached data")
        return(inv)
        }
 

