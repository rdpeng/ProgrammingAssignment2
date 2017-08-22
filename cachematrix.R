#This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) { 
  i <- NULL   #A null vector
  set <- function(y) {  #A function that sets the value of the matrix
    x <<- y
    i <<- NULL
  }
  get <- function() x  #A function that gets the value of the matrix
  setinverse <- function(inverse) i <<- inverse #This sets the inverse
  getinverse <- function() i  #This gets the inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

#Returns inverse of matrix, calculates and sets cache if necessary 
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i #prints the inverse of the matrix
}
