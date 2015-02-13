        ##  a pair of functions that cache the inverse of a matrix.


  ## This function creates a special "matrix" object that can cache its inverse.
  ## so there is this data type with the matrix and its inverse
  ## when you call the makeCacheMatrix() function it creates the datatype we will call an 'adjoint matrix' with the matrix
  ## when you call cacheSolve() the function takes the adjoint matrix datatype from the previous function and 
  ## checks to see if the inverse is there
  ## if the inverse is not there then it will be calculated using the solve() command and the inverse will be printed out
  ## if the inverse is already in the adjoint matrix the message 'getting cached data' will appear. 

makeCacheMatrix <- function(x = matrix()) {
  
    inverse1 <- NULL # sets m the inverse to NULL
    set <- function(y) { #this function sets the value of the matrix
      x <<- y
      inverse1 <<- NULL
    }
    get <- function() x # this function returns the matrix
    setinverse <- function(solve) inverse1 <<- solve #this function sets the inverse
    getinverse <- function() inverse1 #this function returns the inverse
    list(set = set, get = get, # the whole function returns this list of the functions
         setinverse = setinverse,
         getinverse = getinverse)
  }




  ## This function computes the inverse of the matrix inside the object returned by
  ## makeCacheMatrix above. If the inverse has already been calculated (and 
  ## the matrix has not changed), then cacheSolve should retrieve the inverse 
  ## from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x' using the object deal created by makeCacheMatrix()

    inverse1 <- x$getinverse()
    if(!is.null(inverse1)) {
      message("getting cached data")
      return(inverse1)
    }
    data <- x$get()
    inverse1 <- solve(data, ...)
    x$setinverse(inverse1)
    inverse1
  
}
