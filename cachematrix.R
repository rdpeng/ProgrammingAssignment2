## DHWWall4 - Assignment2:  These functions are based on the Assignment examples of makeVector and cacheMean.    
## makeCacheMatrix function:  Accepts a square matrix.  It handles the matrix storage.
## Usage:  meow <- makeCacheMatrix(matrix(c(1,2,4,5,7,9,12,14,22), nrow=3, ncol=3, byrow=TRUE))
makeCacheMatrix <- function(x = matrix()) {
  solvedmatrix <- NULL # set to null
  message("Matrix value: ")
  print(x)
  get <- function() {
    x  # returns value of x from this environment
  }
  setsolvedmatrix <- function(solved) {
    solvedmatrix <<- solved
    solvedmatrix
  }
  getsolvedmatrix <- function() { 
    solvedmatrix  # gets the current value and returns null if no value  
  }
  list(get = get, setsolvedmatrix = setsolvedmatrix, getsolvedmatrix = getsolvedmatrix)
}
## cacheSolve function: Either returns a cached value or runs a new solve() function against the matrix to get its inverse
## Run cacheSolve(meow) twice against a matrix to see it calculated first and then the cache retrieved the second time.
## Usage: cacheSolve(meow)
cacheSolve <- function(x, ...) {
  solvedmatrix <- x$getsolvedmatrix()  # get the current solvedmatrix value from makeCacheMatrix

  if(!is.null(solvedmatrix)) {  #if not null then run return(solvedmatrix) to exit function with value
    message("Cached value found for inverted matrix calculation. Here is cached value: ")
    return(solvedmatrix)
  } else {  
    message("Here is the inverted matrix result from a non-cached calculation: ")
    solvedmatrix <- solve(x$get())  #get the current matrix in x and invert it
    x$setsolvedmatrix(solvedmatrix)
    return(solvedmatrix)
  }
}
