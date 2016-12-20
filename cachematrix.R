## overall description of functions for this assignment
## The functions in this assignment are designed to cache the inverse of a matrix

## Comments describing this function
##makeCacheMatrix: This function creates a special "matrix"object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
 matinverse <-NULL
 set <- function(y){
   x<<- y
   matinverse <<-NULL
 }
 get <-function()x
 setInverse <-function(inverse) matinverse<<-inverse 
 getInverse <-function()matinverse
 list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


##cacheSolve: This function computes the inverse of the special "matrix"returned by makeCacheMatrix
##above. If the inverse has already been calculated (and the matrix has not changed), then cacheSolve
##should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  matinverse <- x$getInverse()
  if(!is.null(matinverse)){
    message("getting cached data")
    return(matinverse)
  }
  matx <-x$get()
  matinverse <-solve(matx, ...)
  x$setInverse(matinverse)
  matinverse
}

