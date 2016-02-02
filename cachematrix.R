## Put comments here that give an overall description of what your
## functions do

## Creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {

s <- NULL
set <- function(y) {
 x <<- y
 s <<- NULL
 }
 get <- function() {
   x
 }
 setSolve <- function(solve) {
 s <<- solve
 }
 getSolve <- function() {
 s
}
list(set = set, get = get, setSolve = setSolve, getSolve = getSolve)
}


# Function to get the inversed matrix from a special object created by makeCacheMatrix.

cacheSolve <- function(x, ...) {
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
