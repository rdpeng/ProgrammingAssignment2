## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    # m : inverse matrix value, x : input variable(type : matrix), y : parameter
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    # setinv : assin inverse matrix to m(m is in parent environment.) 
    setinv <- function(inv) m <<- inv
    getinv <- function() m
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    # input x's value to m
    m <- x$getinv()
    #inverse matrix has already computed, then return stored inverse matrix value
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    #compute inverse matrix
    m <- solve(data)
    #store inverse matrix
    x$setinv(m)
    m
}
