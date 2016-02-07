## Programmer : Osvaldo
## Date : 2/5/2016
## Program name: cachematrix.R
## Purpose: The II programming asignment from Coursera R Studio Module
##          It is required to create function makeCacheMatrix, to assign the inverse of the matrix in memory

makeCacheMatrix <- function(x = matrix()) {
  print(x)                  #Let's print the paramenter
  m <- NULL                 #Create variable m, assigning null value
  set <- function(y) {      #Function to store the value in memory
    x <<- y                 #Double assignment store the value in other environment
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}

##          It is required to create function cacheSolve, to get the solved matrix from memory, avoiding to recalculate it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getsolve()
  if(!is.null(m)) {
    message("getting solved matrix")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}

