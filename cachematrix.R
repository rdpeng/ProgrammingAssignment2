## Our aim in this experiment is to write a pair of functions, namely,
##"makeCacheMatrix" and "cacheSolve" that cache the inverse of a matrix
##makeCacheMatrix is a function which creates a special "matrix" object that can
##cache its inverse for the input (which is an invertible square matrix)

## makeCacheMatrix is a function which creates a special "matrix" object that can
## cache its inverse for the input (which is an invertible square matrix)
## I simply set the input x as a matrix
## and then set the solved value "s" as a null then I changed every 
## reference to "mean" to "solve"

makeCacheMatrix <- function(x = matrix(sample(1:100,9),3,3) {
s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
}
get <- function() x
  setsolve <- function(solve) s <<- solve
  getsolve <- function() s
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}
        

## cacheSolve is a function which computes the inverse of the special "matrix"
## returned by makeCacheMatrix above. If the inverse has already been calculated
## (and the matrix has not changed), then the cachesolve should retrieve the
## inverse from the cache
## Same here, changed "mean" to "solve" and "m" to "s"

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
