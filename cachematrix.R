## Saving on GitHub
## The following program has two functions: 
## makeCacheMatrix: Takes a matrix as input and stores its inverse in cache
## cacheSolve: Takes a matrix as input and checks to see if it's inverse is
##            available in cache otherwise computes the inverse and stores it in
##            cache (using first function). Returns the inverse of the matrix


## Function makeCacheMatrix: Input it a matrix. Has 4 variables:
##  1. get: returns the input matrix
##  2. set: sets the value of the matrix in cache
##  3. getinv: returns the inverse of the matrix
##  4. setinv: sets the inverse of the matrix in cache
##  Returns a list of a combination of the 4 variables (set,get,setinv,getinv)
makeCacheMatrix <- function(x = matrix()) {
  mat <- NULL
  set <- function(y) {
    x <<- y
    mat <<- NULL
  }
  get <- function() x
  setinv <- function(inv) mat <<- inv
  getinv <- function() mat
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)

}


## Function cacheSolve: Input is a matrix
##  Check if it's inverse is available in cache (by calling makeCacheMatrix),
##  if available, return the inverse. Else compute the inverse and return it
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}
