## Patrick King, R Programming (Coursera), R.D. Peng, Programming Assignment 2
## Caching the Inverse of a Matrix

## makeCacheMatrix creates a special "matrix" object that can cache its inverse.

## x passes the matrix itself.
## changed stores whether the matrix has been changed by the set method, initially FALSE.
## invMatrix stores x's inverse as invMatrix when set with setInv, or NULL if not yet set.
## set stores the passed matrix (y) as x,  initializes invMatrix to Null, and sets changed to TRUE.
## get returns the stored matrix. 
## setInv stores a passed marix (inv) as invMatrix, and sets changed back to FALSE.
## getInv returns invMatrix.
## getChanged returns the boolean state of changed.
## makeCacheMatrix doesn't return a matrix object, but rather a list object with 
## the above methods and properties as elements.

## makeCacheMatrix must use the '<<-' operator to set the matrix properties (x and invMatrix)
## to work because the calculated inverse matrix is from a separate environment. Without '<<-'
## (using '<-' instead) invMatrix doesn't seem to get set to the calculated matrix from cacheSolve.

makeCacheMatrix <- function(x = matrix()) {
        changed <<- FALSE
        invMatrix <- NULL
        set <- function(y) {
                oldx <- x
                x <<- y
                invMatrix <<- NULL
                changed <<- TRUE 
        }
        get <- function() x
        setInv <- function(inv) {
                invMatrix <<- inv
                changed <<- FALSE
        }
        getInv <- function() invMatrix
        getChanged <- function() changed
        list(set = set, get = get, setInv = setInv, getInv = getInv, getChanged = getChanged)
}

## cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix. 

## x passes a cacheMatrix list object.
## i gets x's inv value.

## If i has been already been set, and the matrix hasn't changed,
## then out a message that it is cached, then return i. Use is.null to determin NULLness,
## and the TRUE/FALSE state of changed from the x object to determine if the matrix has been reset.

## Otherwise, data gets the x objects stored matrix,
## i gets the calculated inverse of data using solve(),
## the cacheMatrix's inv is set to i, 
## and finally, i is returned.


cacheSolve <- function(x, ...) {
        i <- x$getInv()
        if(!is.null(i) && !x$getChanged()) { 
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setInv(i)
        i
}