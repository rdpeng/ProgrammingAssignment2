## makeCacheMatrix is a function which creates a special "matrix" object that can 
## cache its inverse for the input (which is an invertible square matrix)
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}	
## cacheSolve is a function which computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been calculated and the
## matrix has not changed, then the cachesolve should retrieve the inverse from the cache
cacheSolve <- function(x, ...) {	cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}	}
## Checking the program
## m <- matrix(rnorm(16),4,4)
## m1 <- makeCacheMatrix(m)
## cacheSolve(m1)
##[,1]      [,2]       [,3]         [,4]
##[1,] -0.03715441 1.5372721 -1.4424514  0.651232188
##[2,] -1.48833679 1.5556870 -0.0222469  1.443657052
##[3,]  0.33885798 0.3621433 -1.0732098 -0.008076643
##[4,] -0.75571347 0.7423995 -2.7701083  1.832332023
