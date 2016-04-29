
#### Start Assignment
## These functions create an matrix inversion in the most
## efficient manner possible. Because of the tax that inversions play on a system
## this set of functions will allow the user to cache a matrix rather than continually recalcualte
## the inverse of the same matrix.

# Part 1 ------------------------------------------------------------------

## This function creates a special matrix

makeCacheMatrix <- function(x = matrix()) {
    ## this is a function where the argument x is an invertable matrix
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    ## A function of x is stored in get, a function of inv takes the argument inverse and is
    ## stored in "setinv", getinv takes on the function of the inv matrix. The list provides input for
    ## the cacheSolve function.
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set=set, get=get, setinv=setinv, getinv=getinv)
}

# Part 2 ------------------------------------------------------------------


## This function provides the inverse of the matrix developed above and provides a cashce
## of already inverted matrices so that the fucntion does not waste time with the same answer.

cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if(!is.null(inv)){
        message("cashed data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    return(inv)
}



