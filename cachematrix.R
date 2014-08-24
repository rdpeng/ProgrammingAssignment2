## cachematrix.R
#
# This files contains the following functions:
#   * makeCacheMatrix -- receives a matrix, returns cached matrix, a list
#                        of closures to get/set matrix data and inverse
#   * cacheSolve -- receives a cached matrix, returns inverse, first
#                   setting it, if it is not yet cached
#
# If this file is run, it will create a cached matrix and then run
# `cacheSolve` on it three times. This will show the inverse and
# display a message when the cached inverse is used. This can be done with:
#
# ```bash
# R --vanilla --slave < cacheMatrix.R
# ```
#

## Cached Matrix Builder
#
# Receives an invertible matrix `mtrx`, and returns a
# cached matrix object. This object can be used by `cacheSolve`.
#
# Lexcial scoping is used to reduce memory footprint of copies,
# by creating a cacheable matrix object that closes over
# the matrix data and it's cached inverse.
#
# A cacheable matrix object is a list with the following functions:
#
#   * get -- returns the matrix data
#   * set -- sets the matrix data, clearing the cached inverse
#   * getInverse -- returning the cached inverse
#   * setInverse -- receives the inverse and caches it
#
makeCacheMatrix <- function(mtrx = matrix()) {
    # initialize cached inverse value to null
    inverse <- NULL

    # return a list of getters and setters, closures over mtrx and inverse
    list(
        # returns the matrix data
        get = function() mtrx,
        # clears inverse cache, sets matrix data to `m`, returning it
        set = function(m) {
            inverse <<- NULL
            mtrx <<- m
            mtrx },
        # returns cached inverse value
        getInverse = function() inverse,
        # sets the cached inverse value, returning it
        setInverse = function(i) {
            inverse <<- i
            inverse })
}


## Cached Matrix Solver
#
# This function receives a cached matrix object, `mtrx`, as created by the
# makeCacheMatrix function above and returns it's cached inverse, if it
# it has one; otherwise, it sets the cached inverse to the results of
# `solve` on the matrix data, returning the results.
#
cacheSolve <- function(mtrx) {
    # get the cached inverse value from mat, or null
    inverse <- mtrx$getInverse()

    # if the inverse has been cached, it will not be null, so, print a
    # message and return the cached value
    if(!is.null(inverse)) {
        message("using cached inverse")
        inverse
    }
    # otherwise, set the inverse to the results from `solve` and return
    else mtrx$setInverse(solve(mtrx$get()))
}

## Testing
#
# Evaluate the following likes, e.g. in emacs/ESS with ess-eval-region
# (C-c C-r). Should see cacheSolve run three times, returning the inverse
# of matrix(1:4, 2, 2). After the first evaluation, on the second, and
# third runs, it will print the message "using cached inverse" before.
#
m <- makeCacheMatrix(matrix(1:4, 2, 2))
cacheSolve(m)
cacheSolve(m)
cacheSolve(m)
