## makeCacheMatrix and cacheSolve functions act together to cache and deliver the inverse of a matrix

## This function creates a special "matrix" object that can cache its inverse,  which is really a list of functions to
## 1. set the matrix
## 2. get the matrix
## 3. set the inversed matrix value 
## 4. get the inversed matrix value 

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinv <- function(solve) m <<- solve
    getinv <- function() m
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}

##  This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##  If the inverse has already been calculated (and the matrix has not changed), then cacheSolve 
##  should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    m <- x$getinv()
    if(!is.null(m)) {
        message("Getting Cached Data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinv(m)
    m
}
