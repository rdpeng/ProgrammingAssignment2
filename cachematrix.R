## By the means of functions below special "matrix" object is created 
## that can cache its inverse. Matrix inverse is not calculated but retreived
## from cache

## Creating cache matrix and defining its elements as functions for applying
## to inverse matrix

makeCacheMatrix <- function(x = matrix()) {
m<-NULL
set<-function(y){
  x<<-y
  m<<-NULL
}
get<-function()x
setsolve<-function(solve) m<<-solve
getsolve<-function () m
list(set=set, get=get, setsolve=setsolve, getsolve=getsolve)
}


## Function returns cacheSolve function based on setsolve data from cache
## (instead of recalculating this data)

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
m<-x$getsolve()
if(!is.null(m)){
  message ("getting cached data")
  return(m)
}
data<-x$get()
m<-solve(data, ...)
x$setsolve(m)
m
}
