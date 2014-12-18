## This program creates a R object which holds a matrix data and cache its inverse for future 
## to reduce computational cost

## makeCacheMatrix creates a R object which is nothing but a list of function which 
## stores the matrix in x and initializes the inverse of the matrix as a NULL matrix
## in the variable m. It contains 4 functions to set and get the matrix and to get and
## set the inverse of the matrix. The R object returned by this function is list of 
## functions.

makeCacheMatrix <- function(x = matrix()) {
m <- matrix()
  set <- function(y) {
    x <<- y
    m <<- matrix()
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve is used to calculate the inverse of the matrix x, passed as argument to this 
## function. It first checks whether the inverse is already computed or not. If already computed 
## it returns the cached value otherwise it will compute the matrix inverse using solve() and 
## sets the inverse using setinverse function and returns the inverse of the matrix x

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.na(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
