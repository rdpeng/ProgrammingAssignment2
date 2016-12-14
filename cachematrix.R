## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
#the set function
  set <- function(y)
  {
    x <<- y
    inv <<- NULL
  }
  
  #the get function
  get <- function() x
  
    #setInverse applies the solve function to the matrix 
  setInverse <- function(solve) inv <<- solve(x)
    
    #getInverse returns the inverse value
  getInverse <- function() inv
  
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## cacheSolve takes a special "matrix" object and calculates the inverse if the value is not cached

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  
  if(!is.null(inv))
  {
    print("Getting cached data...")
    return(inv)
  }
  
  data <- x$get()
  inverse <- solve(data)
  x$setInverse(inv)
  
  inverse
}
