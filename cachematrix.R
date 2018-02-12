## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix. 
## The purpose of the function is to cache the inverse of a matrix


## This function creates a special "matrix" object that can cache its inverse.
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of inverse of the matrix
## 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set = set, get = get, setinv = setinv, getinv = getinv)
        
        

}


## The following function computes the inverse of the special "matrix" returned  
## by makeCacheMatrix above. If the inverse has already been calculated (and the
## matrix has not changed), then cacheSolve should retrieve the inverse from
## the cache.

## This function assumes that the matrix is always invertible.
cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinv(inv)
        inv
}
