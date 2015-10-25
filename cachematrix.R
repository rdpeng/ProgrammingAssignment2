## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function


makeCacheMatrix <- function(x = matrix()) 
  #creates a special matrix obect that can cache its inverse
  {
  inv <- NULL
  set <- function(y) #function sets the value of the vector to x {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv #computes the inverse and assignts it into inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {          #if the inversion was already created 
    message("getting cached data.") #it writes the message
    return(inv)                #and returns the inversion
  }
  data <- x$get() # if the inversion wasnt computed it computes the inversion
  inv <- solve(data)
  x$setinverse(inv)
  inv
}
