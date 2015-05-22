## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {   ##this sets the matrix as x
    x <<- y
    m <<- NULL
  }
  get <- function() x           ##This allows you to retreive x 
  setsolved <- function(solve) m <<- solve  ##This allows for the inverse to be solved
  getsolved <- function() m     ##This retieves the solved inverse matrix.
  list(set = set, get = get,
       setsolved = setsolved,
       getsolved = getsolved)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getsolved()
  if(!is.null(m)) {  ##If the matrix is stored in m, it will returned the cached data with the message
    message("getting cached data")
    return(m)
  }
  data <- x$get()   ##If it is not stored it will solve for the inverse of the m atrix
  m <- solve(data, ...)
  x$setsolved(m)
  m
}

x <- makeCacheMatrix()
samplematrix <- matrix(rnorm(9,mean=0,sd=1), 3, 3)  #creates a matrix to test
x$set(samplematrix)
x$get()

cacheSolve(x)
