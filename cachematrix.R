## makeCacheMatrix creates a special "matrix", 
## which is a list including a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse of the matrix
## 4. get the value of the inverse of the matrix 

## The function makeCacheMatrix handles a matrix 
## like an object, with setter and getter methods, 
## furthermore calculates the inverse of the matrix and stores its value
## including setter and getter methods for the calculated inverse.

makeCacheMatrix <- function(x = matrix()) {
  I <- NULL
  
  set <- function(y)
  {
    x <<- y 
    I <<- NULL
  }
  
  get <- function() x 
  setinv <- function(i) I <<- i
  getinv <- function() I 
  list(set = set, get = get, 
       setinv = setinv, 
       getinv = getinv)
  
  
}


## The function cacheSolve can calculate the inverse 
## of the special "matrix" created with the above function.
## However, if the inverse has been calculated then, 
## it gets the inverse from the cache. Otherwise, it computes
## the inverse of the matrix and sets the value of the matrix
## in the cache via the setinv function. 

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  I <- x$getinv()
  if (!is.null(I)) {
    message("getting cache data")  
    return(I)
  }
  
  data <- x$get()
  I <- solve(data, ...)
  x$setinv(I)
  I
}