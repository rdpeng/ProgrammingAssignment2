## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix creates a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of inverse of the matrix
## 4. get the value of inverse of the matrix 

makeCacheMatrix <- function(x = matrix()) {
  inverseObj <- NULL
        set <- function(y) {
                x <<- y
                inverseObj <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inverseObj <<- inverse
        getInverse <- function() inverseObj
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## The following function returns the inverse of the matrix. It first checks if
## the inverse has already been computed. If so, it gets the result and skips the computation. 

cacheSolve <- function(x, ...) {
         ## Return a matrix that is the inverse of 'x'
        inverseObj <- x$getInverse()
        if (!is.null(inverseObj)) {
                message("getting cached data")
                return(inverseObj)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inverseObj)
        inverseObj
}
