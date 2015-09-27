## Put comments here that give an overall description of what your
## functions do

## Do you really need this comment? I believe you're cool enough to understand this without comments ;)

## But, anyway, it is important to comment your code (and also the grade depends on it),
## so here it is: this function, makeCacheMatrix creates a special "vector", which is really 
## a list containing a function to set the value of the vector, get the value of the vector
## set the value of the inverse and get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(solve) inv <<- solve
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## This is a short comment comment, describing this function:
## This function calculates the inverse of the special "vector" created with the above 
## function. However, it first checks to see if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation. Otherwise, it calculates 
## the inverse of the data and sets the value of the inverse in the cache via the setinv function.

## Those comments, they seem to be familiar, aren't they? :)

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
