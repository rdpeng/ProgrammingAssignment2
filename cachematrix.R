## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##Nik's attempt to define a CacheMatrix in R
makeCacheMatrix <- function(x = matrix()) {
  dummymean <- NULL
  setFunction <- function(y=matrix()) {
    placeholder_x <<- y
    dummymean <<- NULL
## x and y variables are set to be matrices
}
  get <- function() x
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  list(setFunction = setFunction, get = get,
       setinv = setinv,
       getinv = getinv)
## solve function chosen on purpose
## so it can take matrices in its function arguments
}
## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
}
