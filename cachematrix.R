## Put comments here that give an overall description of what your
## functions do: 
## First function returns a list which caches a matrix and it's 
## inverse matrix. 
## Second function first checks if the matrix that is passed as 
## it's argument is already cached or not. If cached, it returns the inverse
## matrix from the makeCacheMatrix else it returns the inverse of new 
## matrix. Since we use the solve function to calculate the inverse, the 
## matrix must be square invertible.

## Write a short comment describing this function

## makeCacheMatrix takes any matrix as argument and creates a
## list whose elements are matrices returned by the functions that we define
## within makeCacheMatrix function. 
## set_mat takes as input, the matrix from the enviornment from which it 
## is called and returns it.
## get_mat returns the same matrix. The value it returns is stored by the
## list and used by the second function to evaluate the inverse of the 
## input matrix.
## set_mat_inv returns the inverse of the matrix. Else it stores NULL value
## get_mat_inv returns the inverse of the matrix to the list which is then
## used by second function to compare it it's the same matrix.  
makeCacheMatrix <- function(x = matrix()) {       
  inv_mat <- NULL
  set_mat <- function(y){
   x <<- y
    inv <<- NULL
  }
  get_mat <- function() x
  set_mat_inv <- function(inv) inv_mat <<- inv
  get_mat_inv <- function() inv_mat
  list(set_mat = set_mat, get_mat = get_mat, set_mat_inv = set_mat_inv, 
       get_mat_inv = get_mat_inv)
  
}


## Write a short comment describing this function
## This function takes as argument a square invertible matrix and runs a
## logical operation to check whether the matrix is the same as cached in
## makeCacheMatrix or not. If yes, it returns the inverse from 
## makeCacheMatrix, if not, it goes on to calculate the inverse.
cacheSolve <- function(x,...) {
     ## Return a matrix that is the inverse of 'x'
  inv <- x$get_mat_inv()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data <- x$get_mat()
  inv <- solve(data)
  x$set_mat_inv(inv)
  inv
}
