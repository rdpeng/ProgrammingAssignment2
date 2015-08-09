## The following file creates two functions.  The first function (makeCacheMatrix) creates a cachMatrix object (list)
## that stores a matrix and (potentially) its inverse.  The second function (cacheSolve) computes the inverse of a matrix,
## storing the inverse in its cache.


## makeCacheMatrix creates an object (list) that can store a matrix and its inverse.  It supplies four member functions
## for accessing the data in that object.

makeCacheMatrix <- function(m = matrix()) {

  # Create a holder for the inverse.  Initialize to NULL.
  m_inv <- NULL
  
  # the set function, which stores the matrix in the object.  
  set <- function(new_m) {
    m <<- new_m
    
    # reset the inverse object, since it may be obsolete now.
    m_inv <<- NULL 
  }
  
  # the get function, for accessing the stored matrix
  
  get <- function() m
  
  # function for storing the inverse of the matrix
  
  setInverse <- function(new_inv) m_inv <<- new_inv
  
  # function for accessing the inverse of the matrix
  
  getInverse <- function() m_inv
  
  # return list of functions
  list(set=set, get=get, setInverse = setInverse, getInverse=getInverse)
  
}


## cacheSolve returns the inverse of the matrix in the cacheMatrix object. If it has
## already been solved for, then it simply returns the cached copy.  Otherwise, it
## computes the inverse, stores it in cache, and returns it.

cacheSolve <- function(x, ...) {
  
  # Return a matrix that is the inverse of 'x'
  
  # Check to see if inverse is stored in cache.
  inv <- x$getInverse()

  # If cached version exists, return it.
  if (!is.null(inv)) {
    # Omitting message from example, as it wasn't called for in assignment.
    # Uncomment line below to see message when cache hit occurs.
    # message("getting cached data")
    return(inv)
  }
  
  # retrieve matrix from object
  mat <- x$get()
  
  # compute inverse
  mat_inv <- solve(mat, ...)
  
  # store inverse in cache
  x$setInverse(mat_inv)
  
  # return the computed inverse
  mat_inv
  
}
