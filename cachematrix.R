# John C. McDavid, 09-14-2016
# Programming Assignment 2, R course, Coursera
# "cachematrix.R"
#   two parts (functions):
#     1. makeCacheMatrix - creates a special "matrix" object that can store a matrix and cache its inverse
#     2. cacheSolve - computes inverse of "special" matrix returned by makeCacheMatrix. If inverse
#                       already calc'd and matrix hasn't changed then retrieve from cache
# Adapted from analogous make vector and cache mean, given as example in assignment notes


# function creates a special matrix that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {     # creates an object of class = list - set, get, setinverse, getinverse
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set,
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


# function computes the inverse of teh "special" matrix retunred by makeCacheMatrix. If inverse
#      has already been calc'd and matrix hasn;t changed then get result from cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)) {                           # checks if matrix m exists (not NULL)
        message("getting cached data")
        return(m)                               # if m has something, returns cached data
    }
    data <- x$get()                             # otherwise m is NULL and need to calc inverse
    m <- solve(data, ...)
    x$setinverse(m)
    m                                           # returns calc'd inverse (per solve)
    }
