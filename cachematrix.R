## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## following function are used to create a special object that
##The first function, makeCacheMatrix creates a special “matrix”, which is really a list containing a function to
##stores a matrix and catches its inverse
## i is the inverse function here
##1. set the value of the matrix

## by makeCacheMatrix <- function(x = matrix())
##2. get the value of the matrix

##3. set the value of the inverse

##4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
      x <<- y
      i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set,
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)

}
## Write a short comment describing this function
##This function computes the inverse of the special “matrix” returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if (!is.null(i)) {
      message("getting cached data")
      return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
## end of project
