## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## The function 'makeCacheMatrix' creates a special "matrix", which is actually a list containing functions to
## 1.  set the value of the matrix
## 2.  get the value of the matrix
## 3.  set the inverse of the matrix
## 4.  get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    s <- NULL
    set <- function(y) {
        x <<- y
        s <<- NULL
    }
    get <- function() x
    setSolve <- function(solve) s <<- solve
    getSolve <- function() s
    list(set = set, 
         get = get,
         setSolve = setSolve,
         getSolve = getSolve)
}


## Write a short comment describing this function
## The function 'cacheSolve' calculates the inverse of the special "matrix" created with the function makeCacheMatrix. 
## if the inverse is already available, get the value from the cache;
## Otherwise, calculates the inverse and sets the inverse value in the cache

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    s <- x$getSolve()
    if(!is.null(s)) {
        message("getting cached data")
        return(s)
    }
    data <- x$get()
    s <- solve(data, ...)
    x$setSolve(s)
    s
}
