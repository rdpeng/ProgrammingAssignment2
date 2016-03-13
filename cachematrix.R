#For a given invertible matrix, the following functions calculate or retrieve the inverse matrix from the cache.

#The function "makeCacheMatrix" creates a special "matrix" that can cache its inverse. 
#It contains four functions: set, get, setinverse, getinverse.
#get - returns the matrix x stored in the main function.
#set - changes the matrix stored in the main function.
#setinverse and getinverse - very similar to set and get but they don't calculate the inverse
#they simply store the value of the input in a variable m into the main function (setinverse) and return it (getinverse).

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}

# Function "cacheSolve" computes the inverse of the special "matrix" returned by makeCacheMatrix. 
# If the inverse has already been calculated and the matrix has not changed, 
# then the cachesolve should retrieve the inverse from the cache. 
# If the inverse has not been calculated, data gets the matrix stored with makeCacheMatrix, 
# m calculates the inverse, and x$setinverse(m) stores it in the object m in makeCacheMatrix.

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
