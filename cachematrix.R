## Put comments here that give an overall description of what your
## functions do

makeCacheMatrix <- function(x = matrix()) { ##this function creates a matrix that can cache its inverse
    verse <- NULL
    set <- function(y) {
        x <<- y
        verse <<- NULL
    }
    get <- function () x
    setinv <- function(inverse) verse <<- inverse
    getinv <- function () verse
    list(set = set, 
         get = get,
         setinv = setinv,
         getinv = getinv)
}

cacheSolve <- function(x, ...) { ## this function checks the environment for a chached inverse matrix of the input, and if one is not present then it computes and prints the inverse matrix of the input
    verse <- x$getinv() 
    if(!is.null(verse)) {
        message("getting cached data")
        return(verse)
    }
    dat <- x$get()
    verse <- solve(dat, ...)
    x$setinv(verse)
    verse
}