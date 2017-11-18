## The current script contains two functions:

## makeCacheMatrix returns a 'generalised' matrix object of sorts to the
## parent environment, strictly speaking a list of four functions (closures)
## that in turn instantiate into getters and setters of a user-defined
## numeric matrix or its inverse. These are stored (cached) in the 
## enclosing environement of the listed functions.

## When instantiating makeCacheMatrix, a matrix can be passed directly as
## argument. Otherwise a default one-element (NA) matrix is passed on, until
## concrete matrix values are set via the function get() from the output
## list.

## It is assumed that the supplied matrices are always square and invertible.

## The return list uses as names precisely those of the listed functions,
## to facilitate their calling via the $ operator.

makeCacheMatrix <- function(x = matrix()) {
    inv_matrix <- NULL
    set <- function(matvalues) {
        x <<- matvalues
        inv_matrix <<- NULL
    }
    get <- function() x
    setinverse <- function(inv) inv_matrix <<- inv
    getinverse <- function() inv_matrix
    list(set = set,
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve returns the inverse of the matrix stored in a given
## instance of makeCacheMatrix passed as argument, either by computing and
## storing it prior to display, or directly retrieving it from the 'cache'
## should the inverse have been computed already (for the matrix passed
## in the argument).

## If cacheSolve gets a default instantiation of makeCacheMatrix passed as
## argument, the output will result in a one-element (NA) matrix.

cacheSolve <- function(x, ...) {
    inv_x <- x$getinverse()
    if(!is.null(inv_x)) {
        message("Retrieving cached inverse matrix:")
        return(inv_x)
    }
    my_xmat <- x$get()
    inv_x <- solve(my_xmat)
    x$setinverse(inv_x)
    message("Computing inverse matrix:")
    inv_x
}
