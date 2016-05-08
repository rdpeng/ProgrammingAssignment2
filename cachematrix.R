## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#The function sets and returns the original matrix and its inverse
makeCacheMatrix <- function(x = matrix()) {
   m <- NULL
   Set_matrix <-  function(y) {
   x <<- as.matrix(y)
   m <<- NULL
  }
  Get_matrix <- function() x
  Set_inverse <- function(inv) m <<- inv
  Get_inverse <- function() m
  list(Set_matrix = Set_matrix,
       Get_matrix = Get_matrix,
       Set_inverse = Set_inverse,
       Get_inverse = Get_inverse)
}


## Write a short comment describing this function
# This function gets value from cache or re-calculates the value

cacheSolve <- function(x,...){
  m <- x$Get_inverse()
  if(!is.null(m)){
    message("getting Cache")
    return(m)
  }
  data <- x$Get_matrix()
  m <- solve(data,...)
  x$Set_inverse(m)
  m
}
##data <- makeCacheMatrix(matrix(1:4,2,2))
##cacheSolve(data)
