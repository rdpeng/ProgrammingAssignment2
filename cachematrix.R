## Put comments here that give an overall description of what your
## functions do

## function which creates a special "matrix" object that can cache its inverse for the input

makeCacheMatrix <- function(x = matrix(1:100,9,3,3)) {

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


## function which computes the inverse of the special matrix returned by makeCaceMatrix above
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrive the inverse from cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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
