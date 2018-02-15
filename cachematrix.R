## Caching the Inverse of Matrix
## Rather than compute the inverse of matrix repeatedly, the following 
## functions are used to cache the inverse of matrix  

## makeCacheMatrix function creates a matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) 
  {
        inversemat <- NULL
        set <- function(y) {
                x <<- y
                inversemat <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inversemat <<- inverse
        getinverse <- function() inversemat
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
  }
  
## cacheSolve is a function that computes the inverse of the matrix
## created by above function. If inverse has already been calculated
## and the matrix has not changed, the function will then just retrive
## the inverse matrix from the cache (and not compute it again). 

cacheSolve <- function(x, ...) 
  {
        inversemat <- x$getinverse()
        if(!is.null(inversemat)) {
                message("getting cached data")
                return(inversemat)
        }
        matrixdata <- x$get()
        inversemat <- solve(matrixdata, ...)
        x$setinverse(inversemat)
        return(inversemat)
}

