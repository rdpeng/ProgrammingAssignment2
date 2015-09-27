## Functions creates matrixes and inversing them depending of whether the matrix was inversed from the beginnind or changed.

## The function makeCacheMatrix creates a vector, that a) sets the matrix, b) gets the matrix, c) sets the value of the inversed  matrix and d) get the value of inversed matrix

makeCacheMatrix <- function(x = matrix()) {
s <- NULL
set <- function(y) { 
        x <<- y
        s <<- NULL
}
get <- function () x
setinverse <- function(solve) s <<- solve
getinverse <- function() s
list(set=set, get=get, getinverse = getinverse, setinverse=getinverse)
}


## The function calculates the inverse matrix created with the previous function.  

cacheSolve <- function(x, ...) {
        s <- x$getinverse()
        if(!is.null(s)) {
                return(s)
        }
        data <- x$get()
        s <- solve(data,...)
        x$setinverse(s)
        s
}
