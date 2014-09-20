## makeCacheMatrix creates a matrix that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmtrx <- function(solve) m <<- solve
        getmtrx <- function() m
        list(set = set, get = get,
             setmtrx = setmtrx,
             getmtrx = getmtrx)
}

## cacheSolve computes the inverse of the special "matrix" returned by 
makeCacheMatrix. If the inverse has already been calculated 
(and the matrix has not changed), then the cachesolve retrieves the inverse 
from the cache.

cacheSolve <- function(x = matrix(), ...) {
    m<-x$getmtrx()
    if(!is.null(m)){
      message("loading cached data")
      return(m)
    }
    matrix<-x$get()
    m<-solve(matrix, ...)
    x$setmtrx(m)
    m
}
