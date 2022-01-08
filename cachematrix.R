## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## PROGRAMMING ASSIGNMENT 2 (WEEK 3)

makeCacheMatrix <- function(x = matrix()) {
  invert <- NULL
  set <- function(y) { ## set the value of the vector
    x <<- y
    invert <<- NULL
  }
  get <- function() x ## get the value of the vector
  setinv <- function(inv) invert <<- inv ## set the value of the inverse
  getinv <- function() invert ## get the value of the inverse
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  invert <- x$getinv()
  if(!is.null(invert)) { ## validation for null matrix
    message("getting cached data") 
    return(invert)
  }
  data <- x$get()
  invert <- solve(data, ...) ## calculate matrix inverse
  x$setinv(invert)
  invert ## return inverted matrix
}
