## 	the makeCacheMatrix, function creates a special matrix vector holder refered to as an object that can automatically cache its inverse.


makeCacheMatrix <- function(x = matrix()) {
m <- NULL
set <- function(y)
{
x <<- y
m <<- NULL
}
get <- function() x
setinverse <- function(inverse) m <<- inverse
getinverse <- function() m
list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}
 ## function first refers to see if inverse of the matrix has been calculated. 
 ## If inverse calculated, get the inverse from the cache and skips the computation. 
 ## If inverse not calculated, calculate inverse of the data and set the inverse matrix in the cache using setinverse function.
cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
m <- x$getinverse()
if(!is.null(m)) {
message("getting cached data")
return(m)
}
data <- x$get()
m <- solve(data, ...)
x$setinverse(m)
m
}

