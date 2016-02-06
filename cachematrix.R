## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This is a function that can get a matrix and print out a list that contain set,get,set solve and get solve

makeCacheMatrix <- function(x = matrix()) {
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
       getsolve = getsolve)##finally get a matrix
}


## Write a short comment describing this function
##this is a function that contain a slove function which can return the  inverse of the matrix
cacheSolve <- function(x, ...) {
	 s <- x$getsolve()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setsolve(s)
  s
        ## Return a matrix that is the inverse of 'x'
}
