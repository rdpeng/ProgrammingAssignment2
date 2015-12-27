## function makeCacheMatrix
## based on makeVector
## used to facilitate calculations requiring the matrix inverse
## given an invertable matrix this creates a list with 4 entries
## example of how to use
## a <-makeCacheMatrix(b) where b is an invertable matrix
#  creates the list
## cacheSolve(a) solves for the inverse and stores it in the list
## a$get() returns the matrix b
## a$set(c) allows you to change the matrix b in the list to c
## a$setminv(d) is used by the cacheSolve routine
##              will set the inverse to the specified matrix d
##              note: it can also be used directly but this should
##              be used cautiously, only if you have 
##              previously computed the inverse of the matrix
## a$getminv() returns the inverse of the matrix by 
##             pulling it from the list 
##             if the inverse has not been calculated using cacheSolve
##             it will return NULL

makeCacheMatrix <- function(x = numeric(), ...) {
    minv <- NULL
    set <- function(y) {
        x <<- y
        minv <<- NULL
    }
    get <- function() x
    setminv <- function(solve) minv <<- solve
    getminv <- function() minv
    list(set = set, get = get,
         setminv = setminv,
         getminv = getminv)
}

## cacheSolve function
## based on cachemean
## return the matrix inverse for a matrix that has
## been previously stored in a list using makeCacheMatrix
## if the inverse has already been computed it will return
## it from the list otherwise it will compute it
## and set the minv value in the list
##
## input
## x = list created using makeCacheMatrix from invertable matrix
##
## output
##
## inverse of matrix

cacheSolve <- function(x, ...) {
    minv <- x$getminv()
    if(!is.null(minv)) {
        message("getting cached data")
        return(minv)
    }
    data <- x$get()
    minv <- solve(data, ...)
    x$setminv(minv)
    minv
}
