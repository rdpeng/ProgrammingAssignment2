## Our aim in this experiment is to write a pair of functions, namely,
# "makeCacheMatrix" and "cacheSolve" that cache the inverse of a matrix

# makeCacheMatrix is a function which creates a special "matrix" object that can  
# cache its inverse for the input (which is an invertible square matrix)

# makeCacheMatrix creates a list containing a function to
# 1. setting the value of the matrix
# 2. getting the value of the matrix
# 3. setting the value of inverse of the matrix
# 4. getting the value of inverse of the matrix


makeCacheMatrix <- function(x = matrix()) {
   inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## cacheSolve is a function which computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve the 
## inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached result")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
