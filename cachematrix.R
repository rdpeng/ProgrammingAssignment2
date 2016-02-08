## The funtion bellow has the objective to create an object (matrix) that can cache its inverse

## This code contain four functions: set, get, setinverse and getinverse
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function (w) {
                x <<- w
                m <<- NULL
        }
        get <- function () x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list (set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## this function will compute the inverse of the input matrix of "makeCacheMatrix" following some conditions:
## if the inverse has already been estimated, the idea is the "cacheSolve" should retrive the inverse from the cache
## if the inverse has not been estimated, data will be retrieved from matrix of "makeCacheMatrix"
## m calculates the inverse and x@setmean(m) will store the inverse in the object "m" in the "makeCacheMatrix" 

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if (!is.null(m)) {
                message("retrieving cached data")
                return (m)
        }
        dataset <- x$get()
        m <- solve(dataset, ...)
        x$setinverse(m)
        m
}
