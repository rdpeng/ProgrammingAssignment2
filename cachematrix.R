## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        In <- NULL
        set <- function(y) {
                x <<- y
                In <<- NULL
        }
        get <- function() x
        setIn <- function(inverse) In <<- inverse
        getIn <- function() In
        list(set = set, 
             get = get,
             setIn = setIn,
             getIn = getIn)
}

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        In <- x$getIn()
        if (!is.null(In)) {
                message("obteniendo datos en cache")
                return(In)
        }
        data <- x$get()
        In <- solve(data, ...)
        x$setIn(In)
        In
}
