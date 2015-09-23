## Function Makecachematrix creates a special matrix object containing the functions get, setinverse and getinverse
## The function Cachesolve proces the special matrix object created by Makecachematrix and returns its inverse matrix

## Function Makecachematrix, given a square matrix, creates a list containing three functions 

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL  ## set de variable inv to null
  
  get <- function() x  ##  function that returns de data for the matrix
  setinverse <- function(inverse) inv <<- inverse  ## function that stores de inverse matrix in cache
  getinverse <- function() inv  ## function that returns the value of inv from cache
  list(get = get,               ## list containing the three functions
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## Function Cachesolve, given de special matrix object, calculates de inverse matrix.  
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve 
## retrievs the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()  ## Gets the inverse from cache
  if(!is.null(inv)) {   ## if the inverse matrix has already  been calculated returns the value from cache
    message("getting cached data")
    return(inv)
  }
  data <- x$get() ## if the inverse matrix has not been calculated yet, gets the matrix from the special object  
  inv <- solve(data, ...)  ## calculates de inverse matrix and store de result to inv
  x$setinverse(inv)  ## set de inverse matrix to cache
  inv   ## returns de inverse matrix
    
}
