## Two functions that work together to expedite computing inverted matrices.

## First function makes special object that can cache its inverse. 

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  ## argument is reset here so any stored values in the function are erased
  ## key is that the "<<-" is used which assigns to parent environment, meaning
  ## that the separate "cacheSolve" function can access it.
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  ## Now that x has been reassigned in the parent environment, it is retrieved
  get <- function() x
  ## These nested functions are created to be used by cacheSolve() 
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  ## giving labels in the list allows cacheSolve() to call them with the $ sign
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function checks to see if inverse of matrix from first function cached
## If not, calculates inverse. Takes list from makeCacheMatrix() as args

cacheSolve <- function(x, ...) {
  ## first the value of "i" from makeCacheMatrix object retrieved
  i <- x$getinverse()
  ## checks to see if it is null or not
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  ## if i not null, nested get() from makeCacheMatrix retrieves stored matrix
  data <- x$get()
  ## inverse function calculates inverse
  i <- solve(data, ...)
  ## cacheSolve() caches inverse value in makeCacheMatrix object
  x$setinverse(i)
  ## returns inverse
  i
}
