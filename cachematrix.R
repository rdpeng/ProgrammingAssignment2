## (First all, forgive the grammaticals mistakes,
## i'm not a native english speaker)

## The next two functions give the funcionallity for
## solving de inverse of a matrix without doing it
## more than once: If it was calculated before, then
## you'll get the value cached


## 'makeCacheMatrix' is a function that returns an object
## that can hold the values of a matrix and its inverse
## by providing functions to: get/set the matrix cached,
## and get/set the inverse of that matrix

makeCacheMatrix <- function(x = matrix()) {
    invMatrix <- NULL
    setMat <- function(y) {
        x <<- y
        invMatrix <<- NULL
    }
    getMat <- function() x
    setInv <- function(inverse) invMatrix <<- inverse
    getInv <- function() invMatrix
    list(set = set, get = get,
         setmean = setmean,
         getmean = getmean)
}


## 'cacheSolve' is a function that, given an object 'x' (wich type
## is that returned by 'makeCacheMatrix'), it returns the inverse
## of the matrix first checking if that inverse function is already
## cached, if not, then solves the inverse of the matrix inside 'x',
## it saves it in the cache, and returns it

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverse <- x$getInv()
    
    if(!is.null(inverse))
    {
        return(inverse)
    }
    m <- x$get()
    inverse <- solve(m)
    x$setInv(inverse)
    inverse
}
