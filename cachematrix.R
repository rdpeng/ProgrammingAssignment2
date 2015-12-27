## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function


## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
 inv <- NULL
    #set the value of the matrix
    set_matrix <- function(y) {
        x <<- y
        inv <<- NULL
    }
    #get the value of the matrix
    get_matrix  <- function() x
   # set the value of the inverse Matrix
    setInverse <- function(inverse) inv <<- inverse
   # get the value of the inverse
    getInverse <- function() inv
    list(set_matrix = set_matrix,
         get_matrix = get_matrix ,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getInverse()
    if (!is.null(inv)) {
        message("getting cached data, don't need to calculate ")
        return(inv)
    }
    mat <- x$get_matrix ()
    # calculate the inverse of the matrix by using solve function
    inv <- solve(mat, ...)
    x$setInverse(inv)
    inv
}
