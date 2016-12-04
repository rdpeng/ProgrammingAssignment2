##Calculating the inverse of a matrix is always time consuming. The below pair of functions
##cache the matrix inverse. When the content of matrix has not changed, it would look up in the cache instead of recompute.

## This function creates a special "matrix" object that can cache its inverse.The function really is list containing a function to
##set the matrix
##get the matrix
##set the inverse matrix
##get the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set<- function(y){
    x<<- y
    inv<<- NULL
  }
  get<-function()x
  setinverse<- function(inverse) inv <<- inverse
  getinverse<- function()inv
  list(set=set,get=get,
       setinverse= setinverse,
       getinverse= getinverse)
  
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  nv<- x$getinverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv<- solve(data,...)
  x$setinverse(inv)
  inv  ## Return a matrix that is the inverse of 'x'
}
