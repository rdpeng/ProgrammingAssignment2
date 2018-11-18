## Put comments here that give an overall description of what your
## functions do

## Create the cache for the matrix; assign output to variable to use in cacheSolve
## Ex. mat<-makeCacheMatric(your_matrix)

makeCacheMatrix <- function(x = matrix()) {
  #set the value of the matrix
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  #get the value of the matrix
  get <- function() x
  #set the value of the inverse
  setsolve <- function(solve) m <<- solve
  #get the value of the inverse
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## Solve for inverse of matrix and stores result in cache; will return 'getting cached data' on second run
## Ex. cacheSolve(mat)

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getsolve()
  #message prompt if inverse matrix exists in cache
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  #runs function if inverse matrix is not found in cache
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}
