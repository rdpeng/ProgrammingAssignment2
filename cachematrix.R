## The function calculates the inverse of a matrix supplied and 
## stores the inverse in a cache

## To run the function, 
## source('cachematrix.R')
## matr <- makeCacheMatrix(matrix(c(6,0,0,10), c(2, 2)))
## inv <- cacheSolve(matr)

makeCacheMatrix <- function(x = matrix()) {
        inv_mat <- NULL
        set <- function(y) {
            x <<- y
            inv_mat <<- NULL
        }
        get <- function() x
        set_inverse <- function(inv) inv_mat <<- inv
        get_inverse <- function() inv_mat
        list(set = set,
            get = get,
            set_inverse = set_inverse,
            get_inverse = get_inverse
        )
}


## Calculates the inverse of the supplied matrix. It checks if value is cached
## and if not computes the value and sets it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv_mat <- x$get_inverse()
        if(!is.null(inv_mat)) {
            message("Getting cached data...")
            return(inv_mat)
        }
        data <- x$get()
        inv_mat <- solve(data, ...)
        x$set_inverse(inv_mat)
        inv_mat
}
