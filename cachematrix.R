## functions cache the inverse of a matrix
## limitation: the matrix supplied must be invertible - error handler is not included

## creation of special "matrix" object which is designed to store the matrix and related inversed maxrix 

makeCacheMatrix<- function(x = matrix()) {
  s <- NULL # assigning local m standing for inversed matrix to NULL
  set <- function(y) { # specifying set subfunction
    x <<- y  # assigning to x in containing (global in this case) envinronment given value 
    s <<- NULL	# freeing m globally
  }
  get <- function() x	# subfunction returning initial matrix
  setsolve <- function(solve) s <<- solve	# subfunction "memorazing" or caching invesed matrix
  getsolve <- function() s	# subfunction for extracting saved inversed matrix
  list(set = set, get = get, # setting list of subfunctions and their names
       setsolve = setsolve,
       getsolve = getsolve)
}

## finding of inversed matrix (at first in cache, if not - calculated)

cacheSolve <- function(x, ...) {	# computation of the inverse of the special "matrix"
  s <- x$getsolve() # retrieving saved inversed matrix
  if(!is.null(s)) { # if the inversed value was previously saved
    message("retrieving cache") # indicates the usage of cache, may be omitted
    return(s)
  }
  s <- solve(x$get()) # to evade using intermediary variables saved like matr in equal construct:
  # matr <- x$get()
  # s <- solve(matr)
  x$setsolve(s)	# caching calculated inversed matrix
  s # returning inverted matrix as a rsult of function
}	
