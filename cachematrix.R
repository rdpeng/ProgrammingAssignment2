## Pair of functions that cache the inverse of a matrix.

# This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function (matrix = matrix()) {
    ## stores the inverse value
    inverse <- NULL
    ## set the original matrix and reset inverse
    set <- function(y) {
        matrix <<- y
        inverse <<- NULL
    }
    ## get the original matrix
    get <- function() matrix
    ## set inverse value
    set_inverse <-function(inv) inverse <<- inv
    ## get inverse value
    get_inverse <- function() inverse
    
    ## returns a lost of the 4 functions, this list is the special "matrix"
    list(set = set, get = get,
         set_inverse = set_inverse,
         get_inverse = get_inverse)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already
## been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the  cache.
cacheSolve <- function (special_matrix,...) {
    inverse <- special_matrix$get_inverse()
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    data <- special_matrix$get()
    inverse <-solve(data,...)
    special_matrix$set_inverse(inverse)
    inverse
}  
