## Below are two functions that are used to create a
## special object that stores a square invertible matrix and caches its inverse


## The first function, `makeCacheMatrix` creates a special "matrix", which is
## really a list containing a function to

## 1.  set the square invertible matrix
## 2.  get the square invertible matrix
## 3.  set the inverse of the matrix 
## 4.  get the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
    set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmatrix <- function(solve) m <<- solve
  getmatrix <- function() m
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
}


## The following function calculates the inverse of the special "square invertible matrix"
## created with the above function. However, it first checks to see if the inverse
## has already been calculated. If so, it `get`s the inverse from the
## cache and skips the computation. Otherwise, it calculates the inverse of
## the matrix and sets the value of the inverse in the cache via the `setmatrix`
## function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getmatrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setmatrix(m)
  m
}
