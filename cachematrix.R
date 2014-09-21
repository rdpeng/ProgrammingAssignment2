# makeCacheMatrix creates a special "matrix", which is really a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of the inverse
# 4. get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL  # inverse is initially set the value of NULL
    
    # if this function is called, define x in the global environment and
    # assign the value of y to x, at the same time, set inverse as NULL in global  
    # environment
    set <- function(y) {
    x <<- y            # assign the value of y to x in global environment
    inverse <<- NULL   # inverse becomes NULL in global environment
  }
  
  get <- function() x       
  setsolve <- function(solve) inverse <<- solve # assign the value of solve to inverse(in global environment)
  getsolve <- function() inverse # get the value of inverse 
  
  # list the functions as set, get, setsolve and getsolve
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


# The following function calculates the inverse of the special "matrix" 
# created with the makeCacheMatrix function. However, it first checks to
# see if the inverse has already been calculated. If so, it gets the inverse 
# from the cache and skips the computation. Otherwise, it calculates the inverse
# of the data and sets the value of the inverse in the cache via the setmean function.
cacheSolve <- function(x, ...) {
  inverse <- x$getsolve() # This code calls the function getsolve()
  
  # confirms that the value of inverse is not null, 
  # and returns the value of inverse by using the return() function
  if(!is.null(inverse)) { 
    message("getting cached data")
    return(inverse)
  }
  
  data <- x$get() # get data with the get function
  inverse <- solve(data, ...) # get the inverse of data with solve() function
  x$setsolve(inverse) # the setsolve() is called in the cacheSolve() function
  inverse # return inverse
}
