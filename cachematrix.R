## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix allow to:
## "set" sets the value of the matrix to the passed argument
## "get" gets the current value of the matrix
## "setmatrix" finds the inverse, by passing the result of 
## cacheSolve as the argument
## "getmatrix" gets the inverse of the current matrix 

makeCacheMatrix <- function(x = matrix()) {
  mat <- NULL
  set <- function(y){
    ## set a new matrix and make also sure the inverse in deleted
    x <<- y
    mat <<- NULL
  }
  get <- function(y) x
  setmatrix <- function(solve) mat <<- solve
  getmatrix <- function() mat
  list(set = set, get = get, setmatrix = setmatrix,
       getmatrix = getmatrix)
}


## cacheSolve calculates the inverse of the matrix passed
## as an argument

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  mat <- x$getmatrix()
  if(!is.null(mat)){
    message("getting cached data..")
    return(mat)
  }
  data <- x$get()
  mat <- solve(data, ...)
  x$setmatrix(mat)
  mat
}
