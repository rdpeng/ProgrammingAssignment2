## Coursera Programming Assignment2
## These two functions improve the performance of the R code where repeated tasks take longer time
## these functions are used to cache the operation output and return when the operation is repeated


## Set the value of the Matrix, Get the Value of the Matrix, Set and get the Inverse of the Matrix
makeCacheMatrix <- function(x = matrix()) {
  inver <- NULL
  set <- function(y) {
    x <<- y
    inver <<- NULL
  }
  get <- function() x
  setSolve <- function(inverse) inver  <<- inverse
  getSolve <- function() inver
  list(set = set, get = get,
       setSolve = setSolve,
       getSolve = getSolve)
}

## Write a short comment describing this function

# If inverse is not available in the cache then only solve it
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inver <- x$getSolve()
  if(!is.null(inver)) {
    message("getting cached data")
    return(inver)
  }
  mymatrix_data <- x$get()
  inver <- solve(mymatrix_data, ...)
  x$setSolve(inver)
  inver
}
