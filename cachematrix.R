## The two functions below create a special object that stores a 
## matrix and caches its inverse

## This function returns a special object containing functions to
## 1. Set the value of the matrix
## 2. Get the value of the matrix
## 3. Set the value of the matrix's inverse
## 4. Get the value of the matrix's inverse
makeCacheMatrix <- function(x = matrix()) {
  x_inv <- NULL
  
  set <- function(y) {
    x <<- y
    x_inv <<- NULL
  }
  
  get <- function() x
  
  set_inv <- function(inverse) x_inv <<- inverse
  
  get_inv <- function() x_inv
  
  list(set = set, get = get, set_inv = set_inv, get_inv = get_inv)
}


## This function calcualtes the inverse of a matrix only if the 
## if its not already been calculated 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  mat_inv <- x$get_inv()
  if (!is.null(mat_inv)) {
    message("Getting cached inverse")
    return(mat_inv)
  }
  
  mat_val <- x$get()
  mat_inv <- solve(mat_val, ...)
  x$set_inv(mat_inv)
  mat_inv
}
