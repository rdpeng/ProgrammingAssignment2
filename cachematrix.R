
 ##The makeCacheMatrix are used to create a special object that stores a matix and cathces its inverse.

makeCacheMatrix <- function(x = matrix()) {
 k <- NULL
  set <-function(y) {
    x<<- y
    k<<- NULL
  }
  get <-function()x
  setInverse<-function(inverse) k <<-inverse
  getInverse <-function() k
  list(set=set,
       get=get,
       setInverse = setInverse,
         getInverse = getInverse)
}


## cacheSolve retrives the iverse from cache if the inverse has already been calculated.

cacheSolve <- function(x, ...) {
             ## Return a matrix that is the inverse of 'x'
  cacheSolve <-function(x, ...){
    k <- x$getInverse()
    if(!is.null(k)) {
      message("getting cached data")
      return(k)
    }
    data <-x$get()
    K <-solve(data, ...)
    x$setinverse (k)
    K
  }
  

