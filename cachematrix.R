## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
}
## Put comments here that give an overall description of what your
## functions do
The goal in this activity is to create a couple of functions that restore
or cache the inverse of a matrix
## Write a short comment describing this function
makeCacheMatrix function basically makes a special "matrix" object that can
store its inverse for the data
makeCacheMatrix <- function(x = matrix()) {
inv <- NULL
set <- function(y) {
x <<- y
inv <<- NULL
}
get <- function() x
setInverse <- function(inverse) inv <<- inverse
getInverse <- function() inv
list(set = set,get = get,setInverse = setInverse,getInverse = getInverse)
}


## Write a short comment describing this function
cacheSolve is a function that register the inverse of the special "matrix" restored by makeCacheMatrix. On the off chance that the inverse has been computed, at that point the cacheSolve ought to recover the inverse out of the cache
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
inv <- x$getInverse()
if (!is.null(inv)) {
message("getting cached data")
return(inv)}
mat <- x$get()
inv <- solve(mat, ...)
x$setInverse(inv)
inv}