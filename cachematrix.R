## Put comments here that give an overall description of what your
## functions do

## This functions adds four functions into a list. 
## The first function 'set()' defines a matrix and resets the old inverted matrix to 'null'
## The second function 'get()' prints the data (matrix) that is being inverted.
## The third function 'setinverse()' assigns the inverted data to an object 'm'
## The fourth function 'getinverse()' prints out the inverted matrix (object 'm')
## Its objective is to generate space for another function to save computation time by caching a computationally
## intensive result that is meant to be repeated several times.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This simpler function merely checks (using the 'getinverse()' function, whether there is already an inverse
## matrix associated with the data that is benig imputed. If there already is, then that piece of information 
## is retrieved.
## If that is not the case, then the inverse is calculated using the function 'solve()' and then is associated
## with the function 'setinverse()'.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
          m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
