## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  #vble where to store the cached result:
  cachedInverse <- NULL
  
  #function, reset the matrix (x) content to the matrix sent in the argument (y)
  set <- function(y) {
    x <<- y
    cachedInverse <<- NULL
  
  }
  
  #function, return the content of the matrix (x)
  get <- function() x
  
  #function, call once inverse is calculated so it will be cached as inverse of the matrix (x)
  setInverse <- function(inverse) cachedInverse <<- inverse
  
  #function, return the inverse cached
  getInverse <- function() cachedInverse
  
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  #extract the cached inverse for the "x" matrix from his wrapper
  inv <- x$getInverse()
  #if it was calculated previously, return the cached inverse, otherwise ... continue
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  #get the content of the Matrix from his wrapper object in "data"
  data <- x$get()
  #calculate the inverse of the Matrix 
  inv <- solve(data, ...)
  #store the inverse in the wrapper object for of the Matrix 
  x$setInverse(inv)
  inv
}
