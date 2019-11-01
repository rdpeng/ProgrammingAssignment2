## Put comments here that give an overall description of what your
## functions do

## The function makeCacheMatrix gets a matrix as an input, set the value of the matrix, get the value of the matrix, set the inverse Matrix and get the inverse Matrix. The matrix object can cache its own object. 

makeCacheMatrix <- function(x = matrix()) {
     j <- NULL
     set <- function(y){
       x <<- y
       j <<- NULL
       }
     get <- function()x
     setInverse <- function(inverse) j <<- inverse
     getInverse <- function() j 
     list(set = set, get = get, 
              setInverse = setInverse, 
              getInverse = getInverse)
}




## The function cacheSolve computes the inverse of the special “matrix” returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.


cacheSolve <- function(x, ...) {
   ## Return a matrix that is the inverse of 'x'
       j <- x$getInverse()
       if(!is.null(j)){
         message("getting cached data")
         return(j)
         }
       mat <- x$get()
       j <- solve(mat,...)
       x$setInverse(j)
       j
}

