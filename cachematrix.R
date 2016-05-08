## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## The function sets and returns the original matrix and its inverse
makeCacheMatrix <- function(x = matrix()) {
m <- NULL
   Set_matrix <-  function(y) {
   x <<- as.matrix(y)
   m <<- NULL
  }
  Get_matrix <- function() x
  Set_inverse <- function() m <<- solve(x)
  get_inverse <- function() m
}


## Write a short comment describing this function
##The function returns the cached inverse matrix if present or generates the inverse
cacheSolve <- function(x, ...) {
        m <- x$get_inverse()
  if(!is.null(m)){
    message("getting Cache")
    return(m)
  }
  data <- x$Set_matrix()
  m <- solve(data,...)
  x$Set_inverse(m)
  m
        ## Return a matrix that is the inverse of 'x'
}
