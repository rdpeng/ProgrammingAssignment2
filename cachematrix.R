## makeCachematrix.R
## The following two functions allow a matrix's inversion to be cached for possible  
## further use, thus increasing speed. 
##
## This functionality is created by defining a 'Matrix Object' which has Get and Set
## functions for both the matrix and its inversion.

## Usage 
## - Assume m1 is the matrix to be inverted
## - Create the Matrix object with call:   
##          myMatrixObject <- makeCacheMatrix(m1)
## - The first time cacheSolve is called 
##          cacheSolve(myMatrixObject)
##   it will calculate, store and return the inverted matrix
## - Further calls to cacheSolve will be satisfied by getting the stored inversion.
##   In this situation the message "getting cached data" will appear

## Notes: 
## 1. It is assumed that the input is a square matrix and that it is invertible.
## 2. The matrix inversion is done by the solve function in R.


#----------------------------------------------------------------------------

## This function creates takes a matrix as input and creates a matrix Object 
## with get & set functions for the matrix and its inversion.
##
## It also uses the '<<' operator to create variables for the the next function

makeCacheMatrix <- function(x = matrix()) {
      # Initialise from any previous runs of this function
      m <- NULL
      
      # Define the 'set' & 'get' functions for the matrix, and its inversion
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      get <- function() x
      setinversion <- function(solve) m <<- solve
      getinversion <- function() m
      
      # Return the four functions as the function's result
      list(set = set, 
           get = get,
           setinversion = setinversion,
           getinversion = getinversion)
}


## cacheSolve calculates and returns the Matrix inversion
## Its input is a Matrix object. 
## It determines if the inversion has already been calculated. If so, it simply 
## returns that answer, along with message "getting cached data".
## If it hasnt already been calculated then it does so, and stores the answer
## before returning it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      m <- x$getinversion()
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
      }
      data <- x$get()
      m <- solve(data, ...)
      x$setinversion(m)
      m
}
