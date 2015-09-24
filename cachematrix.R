## The makeCacheMatrix & cacheSolve functions take a square matrix x as input
## and return the inverse of that matrix.

## makeCacheMatrix takes a square matrix x as input, which is intended to be cached.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL ## ensure variable "inv" is set (or reset) to NULL, as a placeholder for future value
    set <- function(y) { ## set matrix x to a new matrix y & reset the inverse
        x <<- y
        inv <<- NULL
    }
    get <- function() x ## returns the matrix x
    setinv <- function(inverse) inv <<- inverse ## sets the inverse, inv, to variable inverse
    getinv <- function() inv ## returns the inverse, inv
    list(set = set, get = get, setinv = setinv, getinv = getinv) ## returns the "special matrix" containing all of the functions just defined
}


## cacheSolve returns a matrix that is the inverse of a square matrix x.
## This function assumes that matrix x is invertible.

cacheSolve <- function(x, ...) {
    inv <- x$getinv() ## get the current inverse of the cached vector
    ## check to see if the inverse has already been calculated:
    if(!is.null(inv)) { ## if inv is not NULL, grab & print the value and skip the rest of the function (note: ! = NOT)
        message("getting data")
        return(inv) ## will exit the function
    }
    data <- x$get() ## grab cached x into the data variable
    setinv <- function(inverse) inv <<- inverse ## sets the inverse, inv, to variable inverse
    inv <- solve(data,...) ## calculate the inverse of cached x
    x$setinv(inv)
    inv
}
