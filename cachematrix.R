## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

	in <- NULL
        set <- function(y) {
                x <<- y
                in <<- NULL
        }
        get <- function() x
        setInv <- function(inverse) in <<- inverse
        getInv <- function() in
        list(set = set, get = get,
             setInv = setInv,
             getInv = getInv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
          ## Return a matrix that is the inverse of 'x'
        in <- x$getInv()
        if(!is.null(in)) {
                message("getting cached data")
                return(in)
        }
        data <- x$get()
        in <- solve(data, ...)
        x$setInv(in)
        in
}
