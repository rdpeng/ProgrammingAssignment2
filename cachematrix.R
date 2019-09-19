## Cache the inverse of a matrix
## This function creates a special "matrix" object that can cache its inverse.

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the
## inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse
## from the cache.


makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set<-function(matrix){
    x<<-matrix
    m<<-NULL
  }
  get <- function() x

    setinverse<- function(inverse) 
        m<<- inverse
    
    getinverse <- function() m
    
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
      
    }






## This function computes the inverse of the special "matrix" created by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then it should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv<-x$getInverse()
    if(!is.null(inv)){
          message("geetting cached data")
          return(inv)
    }
      matrix<-x$get()
      inv<- solve(matrix,...)
      x$setInverse(inv)
      inv
}
