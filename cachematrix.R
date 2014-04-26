## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {


# initialize the stored inverse value to NULL
  inv <- NULL
  
  # to set the value of the matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL   # since the matrix changed
  }
  # to get the value of the matrix
  get <- function() x
  # to set the inverse
  setinv <- function(inv_) inv <<- inv_
  # to get the inverse
  getinv <- function() inv
  
  # return a list of all the above functions
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)  

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        # initialize the stored inverse value to NULL
  inv <- NULL
  
  # to set the value of the matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL   # since the matrix changed
  }
  # to get the value of the matrix
  get <- function() x
  # to set the inverse
  setinv <- function(inv_) inv <<- inv_
  # to get the inverse
  getinv <- function() inv
  
  # return a list of all the above functions
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)  
}
