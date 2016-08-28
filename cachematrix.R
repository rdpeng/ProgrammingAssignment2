## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## this function takes a matrix and returns 4 functions in a list
# a function of setting up a matrix
# a function of getting  that matrix 
# a function of setting the inverse of that matrix
# a function of getting the inverse of that matrix
makeCacheMatrix <- function(x = matrix()) {
  I <- NULL
  
  set <- function(y) {
    x <<- y
    I <<- NULL
  }
  
  get <- function() x
  
  
  setInv <- function(Inv) I <<- Inv
  
  getInv <- function() I
  
  
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}

## this function takes as argument a list of four functions (the output of makeCacheMatrix)
## and calculates the inverse of the matrix if not available
cacheSolve  <- function(x, ...) {
  I <- x$getInv()
  if(!is.null(I)) {
    message("getting cached data")
    return(I)
  }
  data <- x$get()
  I <- solve(data, ...) # solve inverts the matrix
  x$setInv(I)
  I
}
