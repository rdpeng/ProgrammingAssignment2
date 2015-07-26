## This file contains two main function: makeCacheMatrix and cacheSolve 
## The aim of both function is to demonstrate how to use "cached data" using the super assignment operator (<<-) to store
## and access a variable and its value stored in the parent environment.

## The makeCacheMatrix takes as an input a matrix (e.g.: mtx <- matrix(c(-1, -2, 1, 1), 2,2)) and returns a list 
## with 4 functions (s.below). The functions are used to cache a value in m in the parent environment.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL                        # Assigns value NULL to m within the called local function
        set <- function(y) {    
                x <<- y                  # Substitutes the matrix if set is called
                m <<- NULL               # Assigns the NULL value to m so that the former inverse matrix 
                                         # dimishes. The new inverse must be calculated with cacheSolve
        }
        get <- function() x              # get function returns the matrix stored in x
        setsolve <- function(solve) m <<- solve # Stores ("caches") the input value in m into the parent environment  
        getsolve <- function() m         # Returnes the "cached" value in m
        list(set = set, get = get,       # Stores the 4 functions in one list object assigned to makeCacheMatrix
             setsolve = setsolve,
             getsolve = getsolve)
}

## This function receives as input a list object from makeCacheMatrix, checks if a value is stored in m, returns this 
## cached value or calculates the inverse of the matrix stored in x and caches the result in m in the parent environment

cacheSolve <- function(x, ...) {
        m <- x$getsolve()               # Accesses m stored with setsolve in makeCacheMatrix
        if(!is.null(m)) {               # Checks if a value is stored in m
                message("getting cached data")
                return(m)               # If a value is stored it returns m and does not recalculate the inverse
        }
        data <- x$get()                 # retrieves the matrix stored in x (main function) / local assignment
        m <- solve(data, ...)           # calculates the inverse / local assignment
        x$setsolve(m)                   # Stores ("caches") the input value in m into the parent environment, if nothing was stored in m
        m                               # returns m
}
