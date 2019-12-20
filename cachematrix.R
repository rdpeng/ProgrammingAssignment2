## The following functions are meant to cache the calculation of the 
## inverse of a matrix

## This function creates a special "matrix" that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL
  set <- function(y){
    x<<-y
    inv<<-NULL
  }
  get <- function() x  
  setinverse<- function(inverse) inv <<- inverse
  getinverse<- function()inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function calculates the inverse of the special "matrix" created
## with the above function. If the inverse has already been calculated,
## the function gets it from the cache and skips computaion

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)){
    return(inv)
  }
  matrix <-x$get()
  inv<-solve(data,...)
  x$setinverse(inv)
  inv
}
