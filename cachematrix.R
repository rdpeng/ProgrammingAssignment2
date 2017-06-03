## Week 3 Assignment for Coursera R Programming
## This function creates a special "matrix" object that can
## cache its inverse.

makecachematrix <- function(x = matrix()) {
    inv <- NULL           ## initialize inv as NULL to hold the
                          ## value of the inverse matrix
    set <- function (y) { ## define the set function to assign new
        x <<- y           ## value of matrix in parent environment
        inv <<- NULL      ## if there is a new matrix, reset inv to NULL

    }
    get <- function() x   ## define the get function - returns the
                          ## value of the matrix argument

    setinverse <- function(inverse) inv <<- inverse
                          ## assign value of inv in parent environment
    getinverse <- function() inv
                          ## get the value of inv where called
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
                          ## to refer to the functions with $ operator
}


## This function computes the inverse of the special matrix returned
## by the makecachematrix function above

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)) {
          message("getting cached data")
          return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}
