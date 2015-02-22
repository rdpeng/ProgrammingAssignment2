## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  s<- NULL
##set the data and set null to inverse
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
##get the inverse
  get <- function() x
##set the inverse to s
  setsolve <- function(solve)  s<<- solve
##get the inverse
  getsolve <- function() s
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
##get inverse of matrix to s
  s <- x$getsolve()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
##if we have computed the inverse of matrix just return the reslut that stored in s
  }
  data <- x$get()
##else calculate the inverse of the data
  s<- solve(data, ...)
  x$setsolve(s)
  s
  ## Return a matrix that is the inverse of 'x'
}
