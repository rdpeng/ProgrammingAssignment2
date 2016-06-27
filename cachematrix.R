## Functions below cache inverse of the matrix

## This function below contains a list of functions that set a value of the matrix 
## as well as the cache inverse of the matrix.
## Also allows retrival of matrix and the cache inverse of the matrix. 

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y){
        x <<- y
        inv <<- NULL
    }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function () inv
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
  }


## The function caculates the cached inverse of the matrix

cacheSolve <- function(x, ...) {
         inv <- x$getinverse
         if(!is.null(inv)){
                message("getting cached data")
                return(i)
         }
        inv.data <- x$get()
        inv <- solve(idata, ...)
        x$setinverse(inv)
        inv
}
