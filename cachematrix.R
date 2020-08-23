##Two functions that can cache the inverse of a matrix object

#Function that creates a special "matrix" that can cache its inverse

makeCacheMatrix <- function(x = matrix()){      #Define the matrix. Arguments in default mode
  inv <- NULL                                   #If the matrix is not cached, inv in NULL
  set <- function(y){                           #Define the set function          
    x <<- y                                     #The value of x (a matrix) in the environment of set function
    inv <<- NULL                                #Reset the value of inv to NULL in the environment of set function
  }
  ##Define get function. Returns the value of a matrix
  get <- function() {x}
  setInverse <- function(inverse) {inv <<- inverse}      ##Assign value of inv in the parent environment
  getInverse <- function() {inv}                         ##Function that get the value of the inverse of a matrix
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

###Return a matrix that is the inverse of "x"

cacheSolve <- function(x, ...){          ###Function that calculate the inverse of a matrix returned by the makeCacheMatrix function
  inv <- x$getInverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}
