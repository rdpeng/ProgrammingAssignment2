## This pair of function solve and/or catch the inverse of a matrix

## makeCacheMatric create an object that is a list containing two object x (the matrix for witch 
# I want to find the inverse) and invm (the inverse of the matrix)

makeCacheMatrix <- function(x = matrix()) {
        invm <- NULL
        set <- function(y) {
                x <<- y
                invm <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) invm <<- inverse
        getinverse <- function() invm
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve retrieve invm from the makeCacheMatrix environment. If it is NULL, then it calculate
# the inverse of the matrix, if not it retur the inverse (invm) plus a message (getting catched 
# inverse matrix)

cacheSolve <- function(x, ...) {
        invm <- x$getinverse()
        if(!is.null(invm)) {
                message("getting catched inverse matrix")
                return(invm)
        }
        data <- x$get()
        invm <- solve(data, ...)
        x$setinverse(invm)
        invm
}
