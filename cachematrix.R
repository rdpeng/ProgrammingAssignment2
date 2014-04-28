## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##
makeCacheMatrix <- function(x = matrix()) {
v <- NULL
    set <- function(y) {
        x <<- y
        v <<- NULL
    }
    get <- function() x
    setinverse<- function(inverse) v <<-inverse
    getinverse <- function() v
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        v <- x$getinverse()
    if (!is.null(v)) {
        message("cached inverse matrix")
        return(v)
    } else {
        v <- solve(x$get())
        x$setinverse(v)
        return(v)
    }

}
