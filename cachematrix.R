## My aim in this assignment is to write a pair of functions, 
## 'makeCacheMatrix' and 'cacheSolve' that cache the inverse of a matrix

## 'makeCacheMatrix' is a type of function that creates a matrix that can
## cache its inverse for the input 

makeCacheMatrix <- function(x = matrix(sample(1:100,9),3,3)) {
 s <- NULL
 set <- function(y) {
   x <<- y
   y <<- NULL
}
   get <- function() x
   setsolve <- function(solve) s <<- solve
   getsolve <- function() s
   list(set = set, get = get,
        setsolve = setsolve,
        getsolve = getsolve)
}

## 'cacheSolve' is a type of function that computes the inverse of the matrix
## returned by 'makeCacheMatrix'. If the inverse has already been calculated
## then the cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
  s <- x$getsolve()
  if(!is.null(s)) {
    message("getting inversed matrix")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setsolve(s)
  s
}
