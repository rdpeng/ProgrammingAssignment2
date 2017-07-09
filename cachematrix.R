## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function creates an special matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
}
## This special matrix is actually a function that set the value of the matrix
  get <- function() x
  ## This part get the value of the matrix
  setinv <- function(InverseMatrix) inv <<- InverseMatrix
  ## This part of the function set the value of the inverse mean
  getinv <- function() inv
  ## This part of the function get the value of the mean
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Write a short comment describing this function
## This function calculates the inverst of the special matrix created with the above function
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)){
    ## In this part of the function checks if the invese matrix has already calculated
    message ("getting cached data")
    ## If the inverse matrix has already been calculated, this part of the function
    ## gets it from the cache (below command) and skips the computation
    return(inv)
  }
  ## If the inverse matrix has not been calculated, this last part of the function
  ## calculates it and sets the value of the inverse matrix via setinv function
  data <- x$get()
  inv <- solve(data,...)
  x$setinv(inv)
  inv
}
