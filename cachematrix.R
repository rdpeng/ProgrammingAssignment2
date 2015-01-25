#Here the <<- operator has been used to assign a value 
#to an object in an environment that is different from the current environment. 
#Below are two functions that are used to create a special object that stores a 
# matrix and cache's its inverse.

# The first function, makeCacheMatrix creates a special "vector", which is really 
# a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of the inverse to the matrix
# 4. get the value of the inverse to the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


#The following function calculates the inverse of the matrix created with 
#the above function. However, it first checks to see if the inverse has already 
#been calculated. If so, it gets the inverse from the cache and skips the 
#computation. Otherwise, it calculates the inverse of the matrix and sets the 
#value of the inverse in the cache via the setinv function.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  invmat <- solve(mat, ...)
  x$setinv(invmat)
  invmat
}