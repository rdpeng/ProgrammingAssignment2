## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# makecacheMatrix creates a matrix object and sets the functions to interact with the object

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set<- function (y){
    x<<- y
    inv <<- NULL
  }
  get <- function() {x}
  Set_inverse <- function(inverse){inv <<- inverse}
  Get_inverse <- function(){inv}
  list (set = set, get =get, Set_inverse=Set_inverse,Get_inverse = Get_inverse )
}


## Write a short comment describing this function
# cacheSolve sets a pathway for the matrix, sets a default message and solves the inversion

cacheSolve <- function(x, ...) {
  inv<- x$Get_inverse()
  }
  if(!is.null(inv)){
    message ("getting cache data")
    return(inv)}
  
  mat <-x$get()
  inv <- solve(mat, ...)
  x$Set_inverse(inv)
  inv
  
  ## Return a matrix that is the inverse of "x"
  
  test <- makeCacheMatrix(matrix(2:2, ncol=2, nrow = 2))
  
test$Get_inverse()

result <- cacheSolve(test)
result
