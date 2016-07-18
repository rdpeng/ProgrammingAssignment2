## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        i = NULL
       set = function(y){
              x <<- y
              i <<- NULL
       }
       get = function() x
       setInverse <- function(inverse) i <<- inverse
       getInverse <- function() i
       list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
       
} # creates special inverse function that can cashe the inverse of a matrix



cacheSolve <- function(x, ...) {
       i = x$getInverse()
       if(!is.null(i)){
              return (i)
       }
       matty = x$get()
       i = solve(matty, ... )
       x$setInverse(i)
       i
       
}      # computes the inverse of the special matrix returned by makeCacheMatrix. 
       # if the inverse has already been calculated and the matrix has not been changed
       # then cacheSolve should retrive the inverse of from the cache       

