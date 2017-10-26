## the program uses two functions makeCacheMatrix and cacheSolve to cache the inverse of a matrix 

## function makeCacheMatrix creates a special "matrix" object that can cache its inverse, see below...

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) { ## this function changes the matrix stored in makeCacheMatrix 
      x <<- y
      m <<- NULL
    }
    get <- function() x ## this function returns a matrix x stored in makeCacheMatrix
    setinverse <- function(solve) m <<- solve ## store the value of the input in a variable m
    getinverse <- function() m ## return the value of the variable m
    list(set = set, get = get, ## store the above 4 functions in the makeCacheMatrix
         setinverse = setinverse,
         getinverse = getinverse)
}


## Function cacheSolve computes the inverse of the special "matrix", see below...

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    data <- x$get()
    m <- solve(data, ...) ## computes the inverse of a square matrix
    x$setinverse(m)
    m
}