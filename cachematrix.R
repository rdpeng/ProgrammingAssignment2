# R Programming
# Programming Assignment 2


# The first function, makeCacheMatrix creates a special "matrix", which is really a list containing a function to 
# 1.set the value of the vector
# 2.get the value of the vector
# 3.set the value of the mean
# 4.get the value of the mean
makeCacheMatrix <- function( m = matrix() ) {
  
  # initialize the result - inverse matrix to NULL
  i <- NULL
  
  # function to set the matrix to the parent of the current environment with "<<-" operator
  set <- function( n ) {
    m <<- n
    i <<- NULL
  }
  
  # function the get the matrix
  get <- function() m 
  
  # function to set the inverse of the matrix to the parent of the current environment with "<<-" operator
  setinverse <- function(inverse) i <<- inverse 
  
  # function to get the inverse of the matrix
  getinverse <- function() i
  
  # return a list of the four functions
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

# The second function cacheSolve is actually the function which calculates the inverse of the speical "matrix“ created with the above function 
# Howevevcer, it first checks if the invrser has already been calculated. If so, it will gets the inverse from the cache and skips the computation.
# Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache via the setinverse function
cacheSolve <- function(x, ...) {
  
  # get the reverse of the matrix from the special matrix 'x' and assigns it to m
  i <- x$getinverse()
  
  # if m is not null, that means the special matrix already contains the inverse of the matrix, simply return it
  if( !is.null(i) ) {
    message("getting cached data")
    return(i)
  }
  
  # since the inverse of actual has NOT been computed before, get the matrix and assign it to data
  data <- x$get()
  
  # compute the inverse of the matrix using solve()
  i <- solve(data)
  
  # set the result to back to the special matrix, so the computation of the inverse of the matrix can be skipped next time, in other words, caching the result
  x$setinverse(i)
  
  # return the result
  i
}
