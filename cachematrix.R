## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    mInv <- NULL
    ## The Inverse has been set as Null when the makeCacheMatrix object type is first initiated
    
    
    ## The set function is defined to reset x and mInv to the data input and NULL respectively.
    ## This is generally useful when the contents of the matrix or its dimensions are changed, the parent function does not have to be initiated again.
    set <- function(y) {
      ## y takes the input argument of the parent function i.e argument of makeCacheMatrix
      x <<- y
      mInv <<- NULL
      ## The "<<-" is used to store the values within the environment of set()
    }
    
    ##The get function returns the data stored in the variable x
    get <- function() x
    ##The setInv is used to reset the values of the mInv variable in cache when it is re-calculated 
    setInv <- function(newInv) mInv <<- newInv
    ## The getInv function gets the mInv value cached in the makeCacheMatrix object type
    getInv <- function() mInv
    
    ## List returns four functions as a list under makeCacheMatrix object type. The names are defined so the internal functions can be referenced with a "$" sign.
    list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    ## reading the inverse matrix from cache (if it is cached in the environment of x
    mInv <- x$getInv()
    
    ##if inverse is cached in the environment of x, meaning it has been computed before,
    ## mInv would not be null and the inverse can be returned from cache rather than being re-computed
    if (!is.null(mInv)) {
        message("Getting Inverse Matrix from Cache")
        return(mInv)
    }
    ##if inverse is not cached (it is NULL), it would be computed using the 'solve' function
    mInv <- solve(x$get())
    ##The solved inverse matrix is stored in to the environment of x so it can be read from the cache
    ## instead of being re-computed when needed again.
    x$setInv(mInv)
    ##With the last statement, the cacheSolve function returns the inverse matrix
    mInv
}
