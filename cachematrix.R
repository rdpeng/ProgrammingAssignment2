## Put comments here that give an overall description of what your
## functions do
## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix rather than compute it repeatedly


## Write a short comment describing this function
##  This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
         inv <- NULL
         set <- function(y){
                x <<- y
                inv <<- NULL
         }
         get <- function(){x}
         setInverse <- function(inverse) {inv <<- inverse} 
         getInverse <- function() {inv}
         list(set = set, get = get, setInverse = setInverse, getInverse = getInverse) 
}


## Write a short comment describing this function
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated,
##  then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if(!is.null(inv)){
                 message("getting cached data")
                 return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)
        inv 
}
