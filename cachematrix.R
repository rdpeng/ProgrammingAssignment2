# With this functions a matrix is created and its inverse is cached. If we want to 
# calculate inverse of a matrix and it doesn't exist in the cache it will be 
#calcultaed nd saved otherwise it will be loaded from the cache.


makeCacheMatrix <- function(x = matrix()) {
  ## makeCacheMatrix --> Creates a list containing the functions to set and get the values 
  ##                     of a matrix; and the functions to set and get the values of its inverse.
  ## x: matrix
  
  i <- NULL
  set <- function(y) {    # Set the value of the matrix x
    x <<- y
    i <<- NULL
  }
  
  get <- function() x   # get the value of the matrix x 
  
  setinverse <- function(solve) i <<- solve # calculate the inverse of matrix x
  
  getinverse <- function() i # load the inverse of matrix x
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        # x: is a list obtained by calling makeCacheMatrix
  i <- x$getinverse() # Extract the inverse of the matrix 
  if(!is.null(i)) {  # If the inverse exist 
    message("getting cached data")
    return(i) # return it from cache
  }
  # otherwise
  data <- x$get() # get the value of the matrix
  i <- solve(data, ...) # Calculate inverse
  x$setinverse(i) #cache inverse
  i
}
