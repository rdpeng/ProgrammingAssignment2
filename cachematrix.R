# This is an efficient script to calculate and store the inverse of matrices

################################################################################
################################################################################

#  The first function, makeCacheMatrix, creates  
#  a list containing a function to
#  
#  set the value of the matrix
#  get the value of the matrix
#  set the value of the inverse
#  get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


################################################################################
################################################################################

#The following function calculates the inverse of the matrix 
#created with the above function. However, it first checks to see if 
#the inverse has already been calculated. If so, it gets the inverse from 
#the cache and skips the computation. Otherwise, it calculates the 
#inverse of the data and sets the value of the inverse in the cache via 
#the setinverse function.

cacheSolve <- function(x) {
  
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setinverse(m)
  m
  
}