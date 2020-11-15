## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  ##Set the value of matrix
  set <- function(y)
  {
    x <<- y
    m <<- NULL
  }
  ##Get the value of matrix
  get <- function() 
  {x}
  ##Then set the value of inverse
  setinverse <- function(inverse)
  {
    m <<- inverse
  }
  ##Get the value of the inverse 
  getinverse <- function()
  {m}
  ##This list is going to show the result, 
  ##we need to update it
  list(set = set, get = get, 
       setinverse = setinverse, 
       getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  ##Assign the inverse matrix to "m"
  m <- x$getinverse()
  ##If the inverse is not null, that means it's already cached
  ##we can skip the calculation
  if(!is.null(m))
  {
    ##If function goes through CacheSolve, 
    ##this message will show up
    message("getting cached data")
    return(m)
  }
  ##Otherwise, it needs to calculate the inverse of the matrix 
  ##and set the value to inverse of the case
  data <- x$get()
  m <-solve(data, ...)
  x$setinverse(m)
  m
}
