# make cache matrix 
# parameter: x  (matrix)
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  
  #set function
  set <- function(y) {    
    x <<- y
    m <<- NULL
    
  }
  
  get <- function() x
  setInverse <- function(inverse) m <<- inverse
  getInverse <- function() m
  list(set = set, get = get, getInverse = getInverse, setInverse = setInverse)
}

#cache solve
#paramater: x (matrix)
cacheSolve <- function(x, ...) {
  
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
