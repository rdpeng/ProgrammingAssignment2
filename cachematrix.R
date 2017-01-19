# This function creates a special matrix object that can cache its inverse
# makeCacheMatrix creates a list containing a function to do below process :
# set the value & get the value of the matrix,
# set the value & get the value of inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
         inv <- NULL
         set <<- function(y){
         x <<- y
         inv <<- NULL
           }
         get <- function() x
         setinverse <- function(inverse) inv <<- inverse
         getinverse <- function() inv
         list(set = set, get = get, 
              setinverse = setinverse,
              getinverse = getinverse)
}

# This function computes the inverse of the special matrix returned by makeCacheMatrix above.
# If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
# This function assumes that the matrix is always invertible.

cacheSolve <- function(x, ...) {
            inv <- x$getinverse()
             if(!is.null(inv)) {
                     message("getting cached data.")
             return(inv)
             }
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    inv
}
