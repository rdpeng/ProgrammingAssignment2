## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its Inverse.
#y=matrix(rnorm(2500), nrow=50, ncol=50)

makeCacheMatrix <- function(x = matrix()) 
  {
  m<-NULL
  
  set <- function(y){
    x<<-y
    m<<-NULL
  }
  
   #print(set)
  get <- function() x
  setInverse <- function(Inverse) m <<- Inverse
  getInverse <- function() m
  list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
  #print(list(set=set, get=get, setInverse=setInverse, getInverse=getInverse))
}
  
  
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the Inverse of 'x'
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data<-x$get()
  m <- solve(data, ...)
  #print(m)
  x$setInverse(m)
  m
}
