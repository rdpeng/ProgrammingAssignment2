# Put comments here that give an overall description of what your
## functions do
## Programming Assignment Function code added by Raji

## Write a short comment describing this function

## This function creates a matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y){
            x <<- y
            inv <<- NULL
    }
      
    get <- function() x  ##prints the matrix inputted
    setInverse <- function(inverse) inv <<- inverse
    getInverse <- function() inv  ##returns inv
    list(set = set,
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
  
}    
## Write a short comment describing this function
## This function creates inverse of the matrix created by makeCacheMatrix.
## If the inverse is available in cache then it will get inverse from cache 
## rather than computing
    

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(!is.null(inv)){
    print("getting cached data")
    return(inv)  ## if inv is not null retrun data from cache
  }
  mat <- x$get()
  inv <- solve(mat,...) ##if inv is null compute inverse
  x$setInverse(inv)
  inv
}

  
