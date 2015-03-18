## Functions makeCacheMatrix and cacheSolve are used to cache the inverse of a matrix which has not changed so far 
## without calling the actual program.

## makeCacheMatrix create special vectors to set and get the inverse of matrix

makeCacheMatrix <- function(x=matrix()){
  m<-NULL
  set <-function (y){
    x<<-y
    m<<-NULL
  }
  get <- function () x
  setinverse <- function(inverse)m <<- inverse
  getinverse <-function() m
  list(set=set,get=get,
       setinverse=setinverse,
       getinverse=getinverse)
}

## cacheSolve functions caches the inverse of a matrix and 
## use it if the same matrix is called again.

cacheSolve <- function(x, ...){
  m<-x$getinverse()
  if(!is.null(m)){
    message ("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}

