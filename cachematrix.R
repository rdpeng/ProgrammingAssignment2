## This combination of functions (makeCacheMatrix & cacheSolve) utilize the property of 
## lexical scoping within R to first create an R object that stores the matrix and it's inverse. 
## The second function (cacheSolve) retrieves the matrix inverse from the cached value stored
## in the makeCacheMatrix object's environment. 
## Put comments here that give an overall description of what your
## functions do

## Description of makeCacheMatrix:
## makeCacheMatrix initialized objects (m set to null to create an object that will be defined later)
## and x is initialized as a matrix within the fnct argument and doesn't require further initialization.
## set takes an argument named 'y' which is a matrix
## x<<-y assings the value 'y' to the object 'x', as defined in the parent environment (makeCacheMatrix)
## when exicuted, set() assigns the input argumentto the 'x' object in the parent environment and
## assigns the value of NULL to the 'm' object (clearing any 'm' value stored by prior exicution of cacheSolve).
## get() defines the getter using 'x' as described in the parent enviornment (utilization of lexical scoping)
## makeCacheMatrix defines the setter and getter: setmatrix_inverse and getmatrix_inverse, respectively. 
## finally, the function creates a new object bt returning a list(). 
makeCacheMatrix <- function(x = matrix()){
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function()x
  setmatrix_inverse <- function(matrix_inverse) m <<- matrix_inverse
  getmatrix_inverse <- function() m
  
  list(set=set, get = get, setmatrix_inverse = setmatrix_inverse, getmatrix_inverse = getmatrix_inverse)
}

## This function computes the inverse of the special "matrix" retured by makeCacheMatrix.
## If the inverse has already been calculated (and the matrix has not changed)
## then cacheSolve should retrieve the inverse from the cache, thus saving time from 
## not having to recalculate the inverse of the matrix.
## cacheSolve REQUIRES an input argument of type makeCacheMatrix()
## (you can't pass a regular matrix to teh function because the $ in $getmatrix_inverse
## does not work with atomic matricies
## note: cacheSolve() and makeCacheMatrix() work together. cacheSolve() is designed to populate
## data or retrieve the mean from an object of type makeCacheMatrix()

cacheSolve  <- function(x, ...) {
  m <- x$getmatrix_inverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setmatrix_inverse(m)
  m
        ## Return a matrix that is the inverse of 'x'
}
