## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    dimension <<- length(y) ^ 0.5
    x <<- matrix(y,ncol = dimension, nrow = dimension)
    m <<- NULL
  }
  get <- function() x
  setInverseMatrix <- function(InverseMatrix) m <<- InverseMatrix
  getInverseMatrix <- function() m
  list(set = set, get = get,
       setInverseMatrix = setInverseMatrix,
       getInverseMatrix = getInverseMatrix)
}


## Write a short comment describing this function

cacheSolve <- function(x) {
        ## Return a matrix that is the inverse of 'x' Sunil
  InverseMatrix <- x$getInverseMatrix()
  if(!is.null(InverseMatrix)) {
    message("getting cached data")
    return(InverseMatrix)
  }
  data <- x$get()
  
  InverseMatrix <<- solve(data)
  x$setInverseMatrix(InverseMatrix)
  InverseMatrix
  
}
