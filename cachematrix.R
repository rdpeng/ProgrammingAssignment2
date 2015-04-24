## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# makeCacheMatrix creates a special "matrix", very much in the same way 
# make CacheVector creates a special "vector". It contains a list 
# containing a function to do the following:
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse
## get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set, get=get, 
       setinverse=setinverse, 
       getinverse=getinverse)  
}


## Write a short comment describing this function
# This function calculates the inverse a matrix, assuming the provided matrix
# always has an inverse, created with the function 'makeCacheMatrix'.
# It first checks if the inverse has already been calculated and return that value
# if so, avoiding to compute it again. Otherwise, it computes the inverse of the 
# provided matrix and sets its value in the cache using the 'setinverse' function.
# It will print the execution time for both cases:
## If the inverse is cached
## If the inverse is not cached, and needs to be computed.

# The execution time is calculated for both cases. Note that in case the matrix is not very
# big, the results for the execution time might seem the same in both cases.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  ptm <- proc.time()
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("Avoiding inverse computation by getting cached inverse.")
    print('Execution time:')
    print(proc.time() - ptm)
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  message("Computing the inverse of the matrix.")
  x$setinverse(inv)
  print('Execution time:')
  print(proc.time() - ptm)
  inv
}
