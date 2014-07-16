## Author: David Parker
## Date:   2014-07-15
## Course: R Programming (Coursera rprog-005)
## Two functions to create a Matrix and calculate its inverse, caching its value
## to conserve time for repeated requests. These functions were modeled after
## makeVector and cachemean functions provided to the class.

# The first function, makeCacheMatrix creates a special "Matrix", which is really a
# list containing a function to
# set the value of the matrix
# get the value of the matrix
# set the value of the inverse matrix
# get the value of the inverse matrix
# The matrix is stored is retrieved via the mx$get() function
# The inverse matrix is solved/cached via cacheSolve calling mx$setinv(mxinv)
#
setwd("~/GitHub/DataScienceSpecialization/ProgrammingAssignment2")
makeCacheMatrix <- function(mx = matrix()) {
    m <- NULL
    
    # set function() | stores passed square matrix
    set <- function(y) {
        mx <<- y
        m <<- NULL
    }
    
    # get function() | retrieves stored square matrix
    get <- function() mx
    
    # setinv function() | cacheSolve() calls to cache the inverse
    setinv <- function(mxinv) m <<- mxinv
    
    # getinv function() | retrieves cached inverse matrix
    getinv <- function() m
    
    # create list of the above functions
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}

# The following caacheSolve function calculates the inverse of the special 
# "matrix" created with the makeCacheMatrix function. However, it first checks
# to see if the inverse matrix has already been calculated. If so, it gets the
# inverse matrix from the cache and skips the computation. Otherwise, it
# calculates the inverse of the matrix and sets the value of the inverse in
# the cache via the setmean function.

cacheSolve <- function(mx, ...) {
    m <- mx$getinv()
    # Check to see if the inverse was cached
    if(!is.null(m)) {
        message("getting cached inverse matrix")
        return(m)
    }
    # Get the matrix to be solved for the inverse
    data <- mx$get()
    # (assumes the matrix passed is invertible)
    m <- solve(data, ...)
    # Cache the inverse matrix
    mx$setinv(m)
    # Returns a matrix that is the inverse of 'mx'
    m
}
