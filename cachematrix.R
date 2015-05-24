## MakeCacheMatrix sets up functions to manipulate the given matrix
## cacheSolve actually solves for the inverse of the matrix if the answer isnt already in the cache

## This function will take in a matrix and output a vector of a list of functions to set the value of the matrix,
##get the value of the matrix, set the value of the inverse, and get the value of the inverse

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


## This function takes in the vector produces by the above function and uses it to solve the inverse of the given matrix
## It checks the cache first, however, to see if the inverse has already been calculated in which case it will skip the 
## computation step and give the cached answer. Otherwrise it'll solve the inverse and read it into cache

cacheSolve <- function(x, ...) {
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

