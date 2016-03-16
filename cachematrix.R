## Caching the Inverse of a Matrix.

## This function creates a special "matrix" object that can cache its inverse. 
## makeCacheMatrix creates a special "matrix", which is really a list containing a function to
## 1.	set the matrix
## 2.	get the matrix
## 3.	set the inverse of the matrix
## 4.	get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
 inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) inv <<- inverse
    getInverse <- function() inv
    list(set = set,
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.
## Otherwise, it computes the inverse of the data and sets the value of the mean in the cache via the setmean function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
         inv <- x$getInverse()
         if (!is.null(inv)) {
         message("getting cached data")
         return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)
        inv
}
