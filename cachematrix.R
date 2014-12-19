##Just as the assignment requires, my two functions gobble up a matrix, return the 
##inverse of that matrix, and also cache the inverse so it can be retrieved quickly.

## This function gobbles up a matrix (and holds onto it) and sets the inverse as NULL.  It also 
##creates a list of functions that: get the stored matrix, sets the inverse in the cache, and 
##also gets the inverse the  for the next function "solveCache".  In essense, this function
##function delivers a set of functions to solveCache and also stores the values created in solveCache.

makeCacheMatrix <- function(x = matrix()) {
  
  s<-NULL
  
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  
  getoriginalmatrix <- function() {x}
  
  setinverse <- function(inverse) {s <<- inverse}
  
  getinverse <- function() {s}
  
  list(set = set, getoriginalmatrix = getoriginalmatrix, setinverse = setinverse, getinverse = getinverse)
  
}

##This function uses the list of functions created in makeCacheMatrix to test if the inverse
##has been cached. If the cached inverse isn't empty, then solveCache returns the
##inverse.  Otherwise, solveCache retrieves the original matrix stored in makeCacheMatrix, 
##solves for the inverse, sets the inverse into the cache, and then returns to inverse so you can see it.

solveCache <- function(y,...) {
  
  s <- y$getinverse()
  
  if(!is.null(s)){
    
    message("Getting cached inverse")
  
    return(s)
    
  }
    
    originalmatrix <- y$getoriginalmatrix()
    
    s <- solve(originalmatrix,...)
    
    y$setinverse(s)
  
    s        
}
