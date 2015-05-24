## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## The first function, makeCacheMatrix creates a "matrix". This list contains a function to
##	1.	set the value of the matrix
##	2.	get the value of the matric
##	3.	sets the inverse
##	4.	gets the inverse

makeCacheMatrix <- function(x = matrix()) {
i <- NULL
set <- function(y)
{
x <<- y
i <<- NULL
}
get <- function() x
setinverse <- function(inverse) i <<- inverse
getinverse <- function() i
list(set = set, get = get, setinverse = setinverse, getinverse =
getinverse)
}

## Write a short comment describing this function
## This function computes the inverse of the "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
i <- x$getinverse()
if(!is.null(i)) {
message("getting cached data")
return(i)
}
data <- x$get()
i <- solve(data, ...)
x$setinverse(i)
i
}
