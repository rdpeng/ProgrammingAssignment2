## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

        im <- NULL
        set <- function(y) {
                x <<- y
                im <<- NULL
        }
        get <- function() x
        set_inverse <- function(solve) im <<- solve
        get_inverse <- function() im
        list(set = set, get = get,
             set_inverse = set_inverse,
             get_inverse = get_inverse)
}

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
                im <- x$get_inverse()
                if(!is.null(im)) {
                        message("getting cached data")
                        return(im)
                }
                data <- x$get()
                im <- solve(data, ...)
                x$set_inverse(im)
                im
}
