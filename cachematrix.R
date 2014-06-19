## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  
  #set function
  set <- function(y) {    
    x <- y
    m <- NULL
    
  }
  
  get <- function() x
  setInverse <- function(inverse) m <<- inverse
  getInverse <- function() m
  list(set = set, get = get, getInverse = getInverse, setInverse = setInverse)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  m <- x$getInverse()
  
  #check null to return cached data
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  data <- x$get()
  
  #solce data and save it
  m <- solve(data)
  x$setInverse(m)
  m
        
}
