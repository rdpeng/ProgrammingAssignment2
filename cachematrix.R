setwd("~/coursera class")
 
## "x" was set as matrix, "inv" was set as null,changed mean to inverse
makeCacheMatrix <- function(x=matrix()) {
  inv<- NULL
  set <-function(y){
    x <<- y
    inv <<- NULL
  }
  get<-function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <-function() inv
  list(set=set,
       get=get,
       setinverse=setinverse,
       getinverse=getinverse)
}

## "m" was changed to "inv" because inverse
## return matrix inverse of "x"
cacheSolve <-function(x,...) {
  inv<-x$getinverse()
  if (!is.null(inv)) {
    message("Getting cached data")
    reutrn(inv)
  }
  data<-x$get()
  inv<-solve(data, ...)
  x$setinverse(inv)
  inv
}
