
#  This is an example how to caching the inverse of a matrix. Below are two
#  functions that are used to create a special object that stores a matrix
#  and cache's its inverse.

##  The first function - "makeCacheMatrix" creates a list, which contains
##  four functions. These functions do things below:
##   1. function "setMatrix" - set a new matrix
##   2. function "setInverse" - set inverse of a matrix
##   3. function "getMatrix" - get values of a new matrix
##   4. function "getInverse" - get values of inverse of a matrix
##  However, first function checks three conditions:
##   1. check if the object is a matrix
##   2. check if the matrix is a square matrix
##   3. check if the inverse of the matrix exists

makeCacheMatrix <- function(x = matrix()) {
    inverseMatrix <- NULL
    # check if the object is a matrix
    if (!is.matrix(x)) {
        return(message('You don\'t type a matrix object.'))
    }
    # check if the matrix is a square matrix
    if (nrow(x) != ncol(x)) {
        return(message('Matrix isn\'t square.'))
    }
    # check if the inverse of the matrix exists
    if (inherits(try(solve(x), silent=TRUE), what='try-error')) {
        return(message('Matrix isn\'t invertible.'))
    }
    setMatrix <- function(m) {
        x <<- m
        # check if the object is a matrix
        if (!is.matrix(m)) {
            return(message('You don\'t type a matrix object.'))
        }
        # check if the matrix is a square matrix
        if (nrow(m) != ncol(m)) {
            return(message('Matrix isn\'t square.'))
        }
        # check if the inverse of the matrix exists
        if (inherits(try(solve(m), silent=TRUE), what='try-error')) {
            return(message('Matrix isn\'t invertible.'))
        }
    }
    getMatrix <- function() {
        x
    }
    setInverse <- function(inverse) {
        inverseMatrix <<- inverse
    }
    getInverse <- function() {
        inverseMatrix
    }
    list(setMatrix = setMatrix, setInverse = setInverse,
         getMatrix = getMatrix, getInverse = getInverse)
}

##  The following function calculates inverse of the matrix created with
##  the above function. However, it first checks if the user type the matrix
##  object. Then function checks to see if the inverse of the matrix has
##  been already calculated. If yes, it gets the inverse of the matrix from
##  the cache and skips the computation. Otherwise, it calculates the inverse
##  of the matrix, using function "solve()" and sets the values in the cache,
##  using the "setInverse" function from the list created before.

cacheSolve <- function(x, ...) {
    # check if user type the matrix object
    if (!complete.cases(x$getMatrix()[1,1])) {
        return(message('You didn\'t type your matrix.'))
    }
    inverseMatrix <- x$getInverse()
    # check if inverse of a matrix been already calculated
    if (!is.null(inverseMatrix)) {
        message('Getting inverse of a matrix from cache:')
        return(inverseMatrix)
    }
    temp <- x$getMatrix()
    inverseMatrix <- solve(temp, ...)
    x$setInverse(inverseMatrix)
    inverseMatrix
}
