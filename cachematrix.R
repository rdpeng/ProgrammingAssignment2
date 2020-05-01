## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

make_cache_matrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
             }
        get <- function() x
        set_inv <- function(inverse) inv <<- inverse
        get_inv <- function() inv
        list(set = set, get = get,
             set_inv = set_inv,
             get_inv = get_inv)
}


## Write a short comment describing this function

cache_solve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$get_inv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$set_inv(inv)
        inv
}
