## My aim is to write functions, "makeCacheMatrix" and "cacheSolve", 
## that cache the inverse of a matrix.

## "makeCacheMatrix" is a function which creates a special "matrix"
## object that can cache its inverse for the input.

makeCacheMatrix <- function(x = matrix()) {
m <- NULL
set <- function(y) {
x <<- y
m <<- NULL
}
get <- function() x
setInverse <- function(inverse) m <<- inverse
getInverse <- function() m
list(set = set,
get = get,
setInverse = setInverse,
getInverse = getInverse)
}

## "cacheSolve" is a function which computes the inverse
## of the special "matrix" returned by "makeCacheMatrix" above.
## If the inverse has already been calculated, the "cachesolve"
## should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
m <- x$getInverse()
if (!is.null(m)) {
message("getting cached data")
return(m)
}
mat <- x$get()
m <- solve(mat, ...)
x$setInverse(m)
m
}

mat <- matrix(rnorm(9), 3, 3)
mat1 <- makeCacheMatrix(mat)
ANSWER <- cacheSolve(mat1)
ANSWER
