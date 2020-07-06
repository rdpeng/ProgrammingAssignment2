#This code facilites solving computionally-expensive operation (inverse of a matrix)

##Makes a matrix of functions, storing chached info 
##Stores calculated inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function()  x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  
  list(get = get, set = set, setinverse = setinverse, getinverse = getinverse)

}


##Takes as input matrix created by makeCacheMatrix
##If inverse was already computed -> returns the inverse
##Otherwise it computes the inverse and stores it into the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  else {
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    return(inv) 
  }
    
}
