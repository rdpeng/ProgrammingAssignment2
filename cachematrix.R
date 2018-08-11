## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# This function creates a list, inlcuding setting the matirx, getting
# the matrix, setting the inverse of the matrix, and getttin the inverse
# of the matrix
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setslove <- function(reversematrix) m <<- reversematrix
  getsolve <- function() m
  list(set = set, get = get,
       setsolvereverse = setsolvereverse,
       getsolvereverse = getsolvereverse)
   
}


## Write a short comment describing this function
# This function calculates the reverse of the input matrix. Fristly, it will 
#check if the inverse has been calculated. If yes, the reult will be returned.
# Otherwise, the inverse function will be calculated and returned.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getsolvereverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolvereverse(m)
  m
  }

