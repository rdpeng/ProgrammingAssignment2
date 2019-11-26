## The assignment is to write a pair of functions that cache the inverse of a matrix.

## The first function, makeCacheMatrix creates a special "matrix" object that can cache its inverse, which is really a list 
## containing a function to

## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse of the matrix
## 4. get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL    # Initialize the matrix inverse as a null value
  set <- function(y) {    # function to reset the matrix
    x <<- y    # Assign the value of 'y' to 'x, which is in the parent environment
    inv <<- NULL
  }
  get <- function() x     #function to get the value of the matrix
  setinverse <- function(inverse) inv <<- inverse    # Assign the value of the inverse of the matrix
  getinverse <- function() inv    #function to get the value of the inverse of the matrix
  list(set = set, get = get,     #create the list containing the 4 functions
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been 
## calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()    # retrieve the inverse stored in makeCacheMatrix()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)    ## Return the inverse of 'x', which is stored in makeCacheMatrix()
  }
  # if the inverse of the matrix is not cached in makeCacheMatrix(), then:
  data <- x$get()    # Get the matrix store in makeCacheMatrix()
  inv <- Solve(data)    # Calculate the inverse of the matrix
  x$setinv(inv)    # Set the inverse
  inv    ## Return a matrix that is the inverse of 'x'
}