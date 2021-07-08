## Put comments here that give an overall description of what your
## functions do
## Write a short comment describing this function

##A pair of functions that cache the inverse of a matrix

## Creates a special matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()){
  inv <- NULL ## Initialize the inverse property
  set <-function(y){ ## Method to set the matrix
    x <<- y
    inv <<- NULL
  }
  get <- function(){  ## Method the get the matrix
    x
  }
  setinverse <- function(inverse){ ## Method to set the inverse of the matrix
    inv <<- inverse
  }
  getinverse <- function(){ ## Method to get the inverse of the matrix
    inv
  }
   ## Return a list of the methods
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Write a short comment describing this function

## Compute the inverse of the special matrix returned by "makeCacheMatrix"above. 
cacheSolve <- function(x,...){
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) { ## Just return the inverse if its already set
    message("getting cached data")
    return(inv)
  }
   ## Get the matrix from our object
  data <-x$get()
  inv <-solve(data, ...)
  x$setinverse(inv)  ## Set the inverse to the object
  inv ## Return the matrix
}
