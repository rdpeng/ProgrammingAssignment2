#Programming Assignment 2: Lexical Scoping

# makeCacheMatrix:

# This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()){
        m <- NULL
        set <- function(y){
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set,
             get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}

#cacheSolve:
# This function computes the inverse of the special "matrix" returned by
# makeCacheMatrix above. If the inverse has already been calculated (and the
# matrix has not changed), then the cachesolve should retrieve the inverse from
# the cache.

cacheSolve <- function(x, ...){
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached inversed matrix")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}

#inversable matrix form R help
hilbert <- function(n) { i <- 1:n; 1 / outer(i - 1, i, "+") }
h8 <- hilbert(8); h8

#test:
#sh8 <- solve(h8)

makeCacheMatrix.close.function <- makeCacheMatrix(x=h8)
cacheSolve(makeCacheMatrix.close.function)
cacheSolve(makeCacheMatrix.close.function)