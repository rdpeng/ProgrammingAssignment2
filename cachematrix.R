## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
m <- NULL
#Set function
set <- function(y)
{
x <<- y
m <<- NULL
}
 #Get function
get <- function() x
#solve() is used to calculate inverse
setinverse <- function(inverse) m <<- inverse
getinverse <- function() m
list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}




## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
cache_inv_x <- x$getinverse()
 +  if(!is.null(cache_inv_x)) {
 +    message("getting cached data")
 +    return(cache_inv_x)
 +  }
 +  data <- x$get()
 +  cache_inv_x <- solve(data, ...)
 +  x$setinverse(cache_inv_x)
 +  cache_inv_x
 +}
