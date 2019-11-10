##  this function, makeCashMatrix creates a special "vector", which is really a list containing a function to
## 1-set the value of the matrix
## 2-get the value of the matrix
## 3-set the value of the inverse matrix
## 4-get the value of the inverse matri

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
## this function first checks to see if the inverse matrix has already been calculated.
##If so, it gets the inverse from the cache and skips the computation. 
##Otherwise, it calculates the inverse matrix of the data and sets the value of the inversematrix in the cache via the setinverse function.

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
        ## Return a matrix that is the inverse of 'x'
}
