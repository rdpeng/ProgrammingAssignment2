#######################################################################
## Coursera - Course RPROG-005 - Jul/Aug 2014
## Programming Assignment 2 - Title: "Lexical Scoping"
## Student: José Ferreira - Portugal
##
## - Summary :
##     This R file has two functions with the purpose of finding
##     the inverse of an invertible matrix.
##
##     If the inverse had already been calculated, future attempts to
##     find the same inverse will retrieve the inverse from memory
##     (cache) instead of doing the same calculations again. This
##     avoids the cost of compute it repeatedly.
##
## - Names of the functions:
##     'makeCacheMatrix'
##     'cacheSolve'
##
## - Description of 'makeCacheMatrix':
##   ------------------------------
##   This function creates a special matrix which is really a list
##   containing a function that uses the following 4 functions
##
##     1. 'set'    to set the value of the matrix
##     2. 'get'    to get the value of the matrix
##     3. 'setinv' to set the value of the inverse
##     4. 'getinv' to get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    ## Variable to memorize the inverse of the matrix, in the cache
    inv_mat <- NULL

    ## Built-in functions and the use of the operator '<<-'
    set <- function(y) {
        x <<- y
        inv_mat <<- NULL
    }

    get <- function() x

    setinv <- function(inverse) inv_mat <<- inverse

    getinv <- function() inv_mat

    ## Return the list of functions above
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## - Description of 'cacheSolve':
##   -------------------------   
##   This function calculates the inverse of the special matrix
##   created by 'makeCacheMatrix'. However it first checks to see
##   if the inverse has already been calculated. If so, it gets
##   the inverse from the cache and skips the computation. Otherwise,
##   it calculates the inverse of the data and sets the value of the
##   inverse in the cache via the 'setinv' function.

cacheSolve <- function(x, ...) {
    ## Read from memory cache an inverse matrix or a null matrix 
    inv_mat <- x$getinv()

    ## If the inverse was calculated before, simply return it
    if (!is.null(inv_mat)) {
        message("getting cached data")
        return(inv_mat)
    }

    ## If the inverse has not yet been calculated, compute it
    data <- x$get()
    inv_mat <- solve(data, ...)

    ## Store the inverse in the memory cache
    x$setinv(inv_mat)

    ## Return the inverse
    inv_mat
}

## - R console example of computing the inverse of a matrix
##
##   > m <- diag(rep(0.5, 3))     # Create a 3 by 3 invertible matrix m
##   > cm <- makeCacheMatrix(m)   # Create the special matrix cm
##   > cacheSolve(cm)             # First call, the inverse is computed
##        [,1] [,2] [,3]
##   [1,]    2    0    0
##   [2,]    0    2    0
##   [3,]    0    0    2
##   > cacheSolve(cm)             # 2nd call, it is readed from cache
##   getting cached data
##        [,1] [,2] [,3]
##   [1,]    2    0    0
##   [2,]    0    2    0
##   [3,]    0    0    2
##   >
##
#######################################################################
