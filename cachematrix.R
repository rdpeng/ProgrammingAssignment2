## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  ## This function creates a special "matrix" object that can cache its inverse
  
  inv <- NULL                                          ##initialize inv as NULL; will hold value of matrix inverse
  set <- function (y) {                                ## define the set function to assign new
    x<<- y                                             ##value of matrix in parent environment
    inv <<- NULL                                       ##if tehre is a new matrix, reset inv to NULL
  }
  
  get <- function() x                                 ## define the get function - returns value of the matrix argument
  
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
  

}


## Write a short comment describing this function
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed),
## then cacheSolve will retrive the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  inv <- x$getinverse ()
  if (!is.null(inv)) {
    
    message ("getting cached data")
    return(inv)
  }
  
  data <- x$get()
  inv <- solve(data, ...)
  
  x$setinverse(inv)  
  inv
}
