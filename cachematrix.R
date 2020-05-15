

Skip to content
Using Sri Sairam Engineering College - TAP CELL Mail with screen readers
Meet
New
Start a meeting
Join a meeting
Chat

Conversations
Using 0.13 GB
Program Policies
Powered by Google
Last account activity: 0 minutes ago
Open in 1 other location · Details

## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL                              
  set <- function(y) {                     
    x <<- y                             
    inv <<- NULL                        
  }
  get <- function() x                     
  setinverse <- function(inverse) inv <<- inverse  
  getinverse <- function() inv                     
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)  
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
Cache mat.txt
Displaying Cache mat.txt.
