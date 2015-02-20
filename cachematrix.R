
## In order to be able to test my function, I assume I would only have to deal with square invertible
## matrices. Therefore, I use the function solve() instead of a generalised function to
## compute the inverse of a matrix

## the function creates a list object that would call the following routines:
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse of the matrix
## get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y){
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) i <<- solve
  getsolve <- function() i
  list(set= set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}

## The following function calculates the mean of the list "matrix" created with 
## the makeCacheMatrix function, after a check for the result of the solve() function
## applied to the same matrix (as in makeCacheMatrix).
## If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it computes the inverse through the solve() function, and stores the value 
## in the cache via the setsolve function. The result is easily seen when one compares:
## A) - the result of cacheSolve(cached) where cached <- makeCacheMatrix(some matrix)
## B) - the result of cacheSolve(makeCacheMatrix(some matrix))  

cacheSolve <- function(x, ...) {
  i <- x$getsolve()
  if (!is.null(i)){
    message('cached is used')
    return(i)
  }
  message('computing')
  data <- x$get()
  i <- solve(data, ...)
  x$setsolve(i)
  i
}
