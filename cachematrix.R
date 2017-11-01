# PEER GRADED ASSIGNMENT #

# CACHING INVERTED MATRIX  #

# Essentially, the example posted in the instructions on coursera.org do what we are looking to accomplish.
# We just need to modify the functions a little bit to handle matrices and compute the inverses, as 
# opposed to handling vectors and computing the means.

# The makeCacheMatrix function contains many nested functions. 
# The nested functions get/set the values in a matrix "x", and they also set/get the inverse of the matrix   

makeCacheMatrix <- function(x = matrix()) { 
  inv <- NULL
  set <- function(y) {      # Sets the matrix
    x <<- y
    inv <<- NULL
  }
  get <- function() x     # gets the matrix
  setinv <- function(inverse) inv <<- inverse   # sets the inverse
  getinv <- function() inv    # gets the inverse
  
  list(set = set, get = get, getinv = getinv, setinv = setinv)
}

#The cacheSolve function will return the inverse of the matrix cached before (if it's possible).

cacheSolve <- function(x,...) { 
  
  inv <- x$getinv()
  
  if (!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  
  mat <- x$get()
  inv <- solve(mat,...)
  x$setinv(inv)
  return(inv)
  
}

#TEST THE FUNCTIONS

mat1 <- matrix(runif(9),3,3)
mat1

cmat <- makeCacheMatrix(mat1)
cacheSolve(cmat)

# Works! :)