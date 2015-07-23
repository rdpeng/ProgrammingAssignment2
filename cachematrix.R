## Caching the Inverse of a Given Matrix
## If the matrix has not been changed
## and the its inverse has been calculated
## then just return the previously calculated inverse
## else calculate the inverse of the new value and return the computed inverse

## Create an object with that returns
## a list of functions to set and get a matrix
## set a computed inverse of a matrix
## return a cache inverse matrix
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    ## set the value given value
    set <- function(y) {
            x <<- y
            m <<- NULL
    }
    ## Return a x with the previously given value or NULL
    get <- function() x
    ## cache the inverse of a given matrix
    setsolve <- function(solve) m <<- solve
    ## Return a previously saved inverse matrix or NULL
    getsolve <- function() m
    ## Expose the list of the functions to set a matrix
    ## get a matrix
    ## set an inverse of a matrix
    ## get an inverse of a matrix
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}


## Given a matrix
## if the matrix has not been inverted
## compute the inverse the matrix, cache the computed value
## and return the computed value
## otherwise just the return the previoulsy cache matrix
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m = x$getsolve()

    if (!is.null(m) && is.matrix(x) && dim(x) == dim(m) && all(x == m) ) {
          message("getting cached matrix")
          return(m)
    }
    ## calculate the inverse of x
    data <- x$get()
    m <- solve(data, ...) ## compute the inverse and cache the result
    m ## return the inverse of x
}
