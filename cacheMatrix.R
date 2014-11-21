## Coursera: R Programming, Assignment 2
## These Functions does the following:
## makeCacheMatrix: creates a special matrix object that can cache its inverse.
## cacheSolve: uses makeCacheMatrix computes the inverse of the matrix created by makeCacheMatrix above.

makeCacheMatrix <- function(x = matrix()) {
                    i <- NULL  ## initializes as NULL when makeCacheMatrix is called.
                    set <- function(y) {
                      x <<- y  ##  saves the input vector as y
                      i <<- NULL  ## resets i to NULL
                    }
                    get <- function() x  ## original value of matrix
                    setinverse <- function(inverse) i <<- inverse  ##this is called by cacheSolve when cacheSolve is originally called.  
                    getinverse <- function() i ## returns cached values to cacheSolve on subsequent attempts.
                    list(set = set, get = get,      ##list of cached values
                         setinverse = setinverse,
                         getinverse = getinverse)
}

cacheSolve <- function(x, ...) {
                i <- x$getinverse() ##gets matrix from x
                if(!is.null(i)) {   ## determines if cached value is null or not.  on first attempt will be null.
                  message("getting cached data")  ## sends message "getting cached data"
                  return(i)  ##returns inverse of matrix
                }
                data <- x$get()  ##if value of i is initially null we get to this point.
                i <- solve(data, ...)  ## computes the inverse of the matrix and assigns to i
                x$setinverse(i)  ## stores inverse of matrix in makeCacheMatrix
                i  ## return inverse to the original code that called function.
##End
}
