## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function creates a special matrix object stored as a listobject containing functions set(), get(), 
## setsolve() and getsolve()
  makeCacheMatrix <- function(x = matrix()) {
      xinv <- NULL # this is where the result of inversion is stored
      # A setter function, use this to set a matrix to object created by makeCacheMatrix function
      # e.g makeCacheMatrix(testmatrix) # here we work on testmatrix
      # makeCacheMatrix$set(testmatrix1) # here we work on testmatrix1
      set <- function(y) {
	  x <<- y
	  xinv <<- NULL # it also initialises xinv to null
      }

      get <- function() x # return the input matrix
      setInv <- function(inv) xinv <<- inv # set the inversed matrix
      getInv <- function() xinv # return the inversed matrix
      # return a list that contains these functions, so that we can use
      # makeCacheMatrix object like these
      # x <- makeCacheMatrix(testmatrix)
      # x$set(newmatrix) # to change matrix
      # x$get # to get the setted matrix
      # x$setInv # to set the inversed matrix
      # x$getInv # to get the inversed matrix
      list(set = set, get = get,
	       setInv = setInv,
	       getInv = getInv)
  }


  cacheSolve <- function(x, ...) {
      xinv <- x$getInv() # get the inversed matrix from object x
      # it will be null if uncalculated, remember the first line "xinv <- NULL" in the previous function
      if(!is.null(xinv)) { # if the inversion result is there
	  message("getting cached data")
	  return(xinv) # return the calculated inversion
      }
      data <- x$get() # if not, we do x$get to get the matrix object
      xinv <- solve(data) # we solve it
      x$setInv(xinv) # we then set it to the object
      xinv # return the solved result
  
