## Matrix inversion is usually a costly computation.  
## Here, the functions makeCacheMatrix and cacheSolve enable caching
## of the inverse of a matrix so that this inverse can be looked up rather 
## than being computed if it is to be used repeatedly, thereby saving computational time.

## makeCacheMatrix creates a list containing:
## 1) a function to set the value of the matrix (set);
## 2) a function to get the value of the vector (get);
## 3) a function to set the value of the inverse of the matrix (setInverse); and
## 4) a function to get the value of the inverse of the matrix (getInverse).

makeCacheMatrix <- function(x = matrix()) {

  inv <- NULL
  
  # The "set" function
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  # The "get" function
  get <- function() x
  
  # The "setInverse" function
  setInverse <- function(inverse)  inv <<- inverse
  
  # The "getInverse" function
  getInverse <- function() inv
  
  list(set = set, 
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  
}


## cacheSolve is a function that computes the inverse of a matrix using makeCacheMatrix.
## If the inverse has already been computed, the function will get the inverse from the cache and skip the computation.
## Otherwise, it will compute the inverse of the matrix and set the inverse in the cache using the setInverse function. 

cacheSolve <- function(x, ...) {
  
  ## x is the list obtained using the function makeCacheMatrix on a matrix A
  ## Return a matrix that is the inverse of A
    
  inv <- x$getInverse()
  
  if (!is.null(inv)) {
    message('Getting cache data...')    
    return (inv)
  }
  
  mat <- x$get()
  inv <- solve(mat)
  x$setInverse(inv)
  
  return(inv)
  
}
