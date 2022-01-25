## Below are two functions:

## 1st = `makeCacheMatrix`: This function creates a special "matrix" object
##                           that can cache its inverse.

## 2nd = .  `cacheSolve`: This function computes the inverse of the special
##                       "matrix" returned by `makeCacheMatrix` above. If the inverse has
##                        already been calculated (and the matrix has not changed), then
##                       `cacheSolve` should retrieve the inverse from the cache.



## makecacheMatrix

makeCacheMatrix <- function(x = matrix()) {
    s <- NULL
    set <- function(y) {
        x <<- y
        s <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) s <<- solve
    getinverse <- function() s
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve


cacheSolve <- function(x, ...) {
    s <- x$getinverse()
    if(!is.null(s)) {
        message("getting cached data")
        return(s)
    }
    data <- x$get()
    s <- solve(data, ...)
    x$setinverse(s)
    s
}
















