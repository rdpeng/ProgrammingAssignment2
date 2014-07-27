## makeCaheMatrix does the following:
##1. set  the Matrix
##2. get the Matrix
##3. set the Inverse of Matrix
##4. get the inverse of the Matrix

makeCacheMatrix <- function(x = matrix()) { 
  m <- NULL ## set the initial value of m to NULL
  ## 1. Set the Matrix
  set <- function(y) { ## saving the matrix to be used in the other environment for it to be cached later
    x <<- y ## assign the matrix to x. Matrix was defined in an external environment
    m <<- NULL ## m has to be null so it doesn't take previously stored values. This resets its values
  }
  ## 2. Get the Matrix
  get <- function() x ## Get the matrix through get function being created here.
  ## 3. Set the inverse of Matrix
  setInverse <- function(solve) m <<- solve ##set the Inverse of the matrix through the solve function
  ##4. Get the inverse of the Matrix
  getmatrix <- function() m ## get the inverse of the matrix through getmatrix
  list(set = set, get = get,
       setInverse = setInverse,
       getmatrix = getmatrix)
}


##cacheSolve calculates the inverse of the matrix created by the makeCacheMatrix. It first checks to see if the inverse of the
##matrix has already been calculated. If so it gets the inverse from the cache and skips the computation.
##Else it calculates the inverse of the matrix and sets the inverse in the cache through the setInverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getmatrix() ## saves the matrix in m for future reference aka caches it
 ## check if m has a previously stored value of not. 
  if(!is.null(m)) {
    message("getting cached matrix")
    return(m)
  }
 ## save the matrix in data 
  data <- x$get()
 ## Compute the inverse if not stored in cache
  m <- solve(data, ...)
  x$setInverse(m)
  m
}
