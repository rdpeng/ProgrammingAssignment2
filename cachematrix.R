#R-Programming
#Programming Assignment 2
#by Diego Horna Munoz

#Clearing the workspace
rm(list=ls())

#Setting the working directory
setwd(dir="/Volumes/Diego/R/ProgrammingAssignment2/")

#Verifying the working directory
getwd()

# This function creates a special matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}

# This function calculates the inverse of a special matrix created by the 
# above function

cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data,...)
    x$setinv(inv)
    inv
}
