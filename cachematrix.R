## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## The makeCacheMatrix is written to create a special "matrix"
## that really is a list containing functions to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  S <- NULL
  set <- function(y) {
    x <<- y
    S <<- NULL
  }
  get <- function() x
  setSolve <- function(Solve) S <<- Solve
  getSolve <- function() S
  list(set = set, get = get,
       setSolve = setSolve,
       getSolve = getSolve)
}


## Write a short comment describing this function

## The following function computes the inverse of the 
## special "matrix" returned by the above function.
## Particularly, it first checks if the inverse has already
## been calculated. If so, it gets the inverse and returns it.
## Otherwise, it calculates the inverse of the data and sets 
## the value of the inverse in the cache via the setSolve function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        S <- x$getSolve()
        if(!is.null(S)) {
            message("getting cached data")
            return(S)
        }
        data <- x$get()
        S <- solve(data, ...)
        x$setSolve(S)
        S
}
