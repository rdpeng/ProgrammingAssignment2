## Put comments here that give an overall description of what your
## functions do

## Inverse of Matrix:CacheMatrix
Catching the inverse of a matrix includes two functions as: makeCacheMatrix() and cachSolve().
*) makeCacheMatrix():
It includes the set,get, setmean and getmean functions.
(1)	Get() returns the vector x stored in the main function.
(2)	Set()  changes the vector stored in the main function.
(3)	Setmean() and getmean() are similar to set and get and  don't calculate the mean, it store the output value in m.

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


## Write a short comment describing this function
cacheSolve():
computes the inverse of the special "matrix" (which is the input of cachemean) returned by makeCacheMatrix(). If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache. If the inverse has not been calculated, data gets the matrix stored with makeCacheMatrix, m calculates the inverse, and x$setmean(m) stores it in the object m in makeCacheMatrix.

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
#Example:
a <- diag(5,3)
a
CachedMarix <- makeCacheMatrix(a)
cacheSolve(CachedMarix)
