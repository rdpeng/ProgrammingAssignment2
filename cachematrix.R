## The first function creates a matrix and can cache the inverse of the matrix 
## The second function returns the inverse of the matrix returned by the first function when the inverse has been calculated.

## This function creates a special matrix to (1) Set the value of the matrix
##(2) get he value of the matrix (3)set the inverse of the matrix (4) get the inverse of the matrix

makeCacheMatrix <- function(M = matrix()) {
        m <- NULL
        set <- function(N) {
                M <<- N
                m <<- NULL
        }
        get <- function() M
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function calculates the inverse of the matrix created with makeCacheMatrix function
##Checked first to see if the value is stored in cache

cacheSolve <- function(M, ...) {
        m <- M$getinverse()
                if(!is.null(m)) {
                        message("getting cached data")
                        return(m)
                }
                data <- M$get()
                m <- inverse(data, ...)
                x$setinverse(m)
                m
}

