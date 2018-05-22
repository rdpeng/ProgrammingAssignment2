## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  
  inversa <- NULL
  set <- function(y) {
    x <<- y
    inversa <<- NULL
  }
  get <- function() x #get the value of the Matrix
  setInverse <- function(inverse) inversa <<- inverse  #set the value of the invertible matrix
  getInverse <- function() inversa  #get the value of the invertible matrix
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inversa <- x$getInverse()
  if(!is.null(inversa)) {  #if inverse matrix is not NULL
    message("getting cached data") #Type message: Getting Cached Invertible Matrix 
    return(inversa)     #return the invertible matrix
  }
  data <- x$get()  #get the original Matrix Data 
  inversa <- solve(data, ...)  #use solve function to inverse the matrix
  x$setInverse(inversa)   #set the invertible matrix 
  inversa  #return the invertible matrix
}
