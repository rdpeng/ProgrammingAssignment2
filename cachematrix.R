## This piece of R code contains 2 functions that caches and returns the inverse of a matrix.
## The 2 functions are: (1) makeCacheMatrix and (2) cacheSolve
## makeCacheMatrix creates a special "matrix" object that can cache the inverse of the "matrix" object.
## cacheSolve computes the inverse of the special "matrix" returned by the makeCacheMatrix.
## Note: If the inverse has already been calculated (and the matrix has not changed), cacheSolve would retrieve the inverse from the cache.


## The makeCacheMatrix function creates a list containing a function that would do the following
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of inverse of the matrix
## 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  my_matrix_inv <- NULL
  set <- function(y) {
    x <<- y
    my_matrix_inv <<- NULL
  }
  get <- function() x
  setinverse <- function(matrix_inverse) my_matrix_inv <<- matrix_inverse
  getinverse <- function() my_matrix_inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## The cacheSolve function returns the inverse of the matrix input into it. It first checks if
## the inverse has already been computed. If so, it gets the result and skips the computation.
## If not, it computes the inverse, sets the value in the cache via setinverse function.

## For this function to return a valid value, the matrix must be a square matrix and its determinant must
## not be zero. The function checks if the matrix is a square matrix and if its determinant is zero

cacheSolve <- function(x, ...) {
  my_matrix_inv <- x$getinverse()
  if(!is.null(my_matrix_inv)) {
    message("Cached data exist......getting cached data.")
    return(my_matrix_inv)
  }
  is_a_square_matrix = TRUE
  input_matrix <- x$get()
  if(nrow(input_matrix) != ncol(input_matrix)){
    is_a_square_matrix = FALSE
  }
  if(is_a_square_matrix == FALSE){
    msg<-paste("The input matrix is not a square matrix. The input matrix must be a square matrix")
    return(msg)
  }
  else{
    if(det(input_matrix)==0){
      msg<-paste("The determinant of the input matrix is zero. Inverse operation is not possible.")
      return(msg)
    }
    else {
      my_matrix_inv <- solve(input_matrix)
      x$setinverse(my_matrix_inv)
      return(my_matrix_inv)
    }
  }
}
