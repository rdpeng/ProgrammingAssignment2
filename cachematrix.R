## These two functions together store a matrix and cache its inverse. 
## The description has been blatantly (with minor modifications) cribbed from the text of the assignment 
## (so has the code).
## Nevertheless, I hope that the explanation will be clear.
## As in the assignment description, the matrix is assumed to have an inverse.

## The first function, makeCacheMatrix, creates a special "matrix", which is really a list
## containing a function to:
##  - set the value of the matrix
##  - get the value of the matrix
##  - set the value of the inverse
##  - get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) i <<- solve
        getinverse <- function() i
        list = (set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## The second function, cacheSolve, calculates the inverse of the matrix. However, it first checks whether the matrix has been already calculated. If it has, it extracts the inverse from the cache. If it hasn't, it calculates the inverse and stores it in the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
