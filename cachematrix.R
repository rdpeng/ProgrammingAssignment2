## below function try to cachse the Inverse of a matrix

## functions are used to creat a special object that store 
## a matrix and cahes its interse

## function creates a "matrix" object that can cahse its inverse.

makeCacheMatrix <- function(x = matrix()) {
         inv <- NULL
         set <- function(y){
           x <<- y
           inv <<- NULL
         }
         get <- function()x
         setInverse <- function(inverse) inv <<- inverse
         getInverse <- function() inv
         list(set = set,
              get = get,
              setInverse = setInverse,
              getInverst = getInverse)
}


##this function compute the inverse of the "matrix" created by 
## makeCacheMatrix. If the inverse has been calculated, then it
## should retrieve the inverse from the cache. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if (!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)
        inv
}
