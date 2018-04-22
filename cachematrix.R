##  We will write a pair of functions that cache the inverse of a matrix, "makeCacheMatrix" and "cacheSolve"


## makeCacheMatrix creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
+ inv <- NULL
+ set <- function(y){       
+ x <<- y
+ inv <<- NULL
+ }
+ get <- function() x
+ setInverse <- function(solveMatrix) inv <<- solveMatrix
+ getInverse <- function() inv
+ list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix above.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
+ inv <- x$getInverse()
+ if(!is.null(inv)){
+ message("getting cached data")
+ return(inv)
+ }
+ data <- x$get()
+ inv <- solve(data,...)
+ x$setInverse(inv)
+ inv 
}
#program check
matrix <- makeCacheMatrix(matrix(data = (1:4), nrow = 2, ncol = 2))
matrix$get()
     [,1] [,2]
[1,]    1    3
[2,]    2    4
cacheSolve(matrix)
     [,1] [,2]
[1,]   -2  1.5
[2,]    1 -0.5
