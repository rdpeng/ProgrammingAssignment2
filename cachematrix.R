## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
}

makeCacheMatrix <- function(x = matrix()) {
m <- NULL
set <- function(y) {
x <<- y
m <<- NULL
}
get <- function() x
setinverse <- function(inverse) m <<- inverse
getinverse <- function() m
list(set = set, get = get,
setinverse = setinverse,
getinverse = getinverse)

}

Write a short comment describing this function

##comment

The following cacheSolve function calculates the inverse matrix of the special "matrix"

created with the above makeCacheMatrix function.

It will first check to see if the inverse has already been calculated. If so, it gets

the inverse from the cache and skips the computation. Otherwise, it calculates the inverse

of the data and sets the value of the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
m <- x$getinverse()
if(!is.null(m)) {
message("getting cached data")
return(m)
}
data <- x$get()
m <- solve(data)
x$setinverse(m)
m
}