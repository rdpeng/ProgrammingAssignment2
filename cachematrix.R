## Put comments here that give an overall description of what your
## functions do

## First identifies if there is an inverse of a matrix has been calculated
## creates a variable containing the inverse of a matrix, using the solve function

makeCacheMatrix <- function(x = matrix()) {
 inv<- NULL
     set<- function(y) {
         x<<- y
         inv<<- NULL
         }
 get<- function()x
      setinverse<- function() inv <<- solve(x)
      getinverse<- function() inv
      list(set = set, 
           get = get, 
           setinverse = setinverse, 
           getinverse = getinverse)
}


## Writes a cache to store a previously prepared inverted matrix


cacheSolve <- function(x, ...) {
       inv<- x$getinverse()
       if(!is.null(inv)) {
           message ("getting inverse matrix")
           return (inv)
       }

	data<- x$get()
     		inv<- solve(data, ...)
     		x$setinverse(inv)
     		inv

}


