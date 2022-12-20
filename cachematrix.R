
## Test change done on 20th December 2022 by RR

## There are two functions in this file 

## First one is makeCacheMatrix. This one takes an argument of a matrix and 
## stores it in local function environment. And the value persists even after 
## completion of the function execution.
## And this function returns four functions. These functions can set and get 
## the matrix. And also set and get the matrix inverse

## Second one is cacheSolve 
## This function computes and returns the inverse of the matrix 
## This one uses makeCacheMatrix to store and retrieve the once-computed inverse
## And avoids recomputing the inverse


## Sample test code below
## mat <- matrix(sample.int(10,9),3,3)
## x <- makeCacheMatrix(mat)
## mat_inv <- cacheSolve(x)
## mat_inv <- cacheSolve(x)
## mat %*% mat_inv



## Stores the passed matrix locally and returns four functions. To set and get 
## matrix. And to set and get the matrix inverse
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set_matrix <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get_matrix <- function() x
  set_inverse <- function(computed_inverse) inverse <<- computed_inverse
  get_inverse <- function() inverse
  list(set_matrix = set_matrix, get_matrix = get_matrix,
       set_inverse = set_inverse,
       get_inverse = get_inverse)
}

## returns the inverse of the matrix. First time it computes the matrix inverse.
## But second time, it just returns the cached inverse
cacheSolve <- function(x, ...) {
  inverse <- x$get_inverse()
  if(!is.null(inverse)) {
    message("getting cached matrix inverse")
    return(inverse)
  }
  data <- x$get_matrix()
  inverse <- solve(data, ...)
  x$set_inverse(inverse)
  inverse
}
