## Put comments here that give an overall description of what your
## functions do

## test

makeCacheMatrix <- function(x = matrix()) { det(x)
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmatrix <- function(matrix) m <<- mean
  getmatrix <- function() m
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
  
  
}


## test
cacheSolve <- function(x, ...) {
  solve(x)
}

