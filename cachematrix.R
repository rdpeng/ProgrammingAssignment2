## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## The function below creates a function that inverts a matrix for a given input
makeCacheMatrix <- function(x = matrix()) {
 m<-NULL
  set<-function(y){
        x<<-y
        m<<-NULL
  }
  get <-function() x
  setinverse<-function(solve) m<<-solve
  getinverse<-function() m
  list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


## Write a short comment describing this function
## The function below uses value of m to evaluate if matrix is inverted, if not it calls the invert function and inverts the matrix
cacheSolve <- function(x, ...) {
m<-x$getinverse()
  if(!is.null(m)){
       message("getting cached data")
       return(m)
  }
  data <-x$get()
  m<-solve(data,...)
  x$setinverse(m)
        ## Return a matrix that is the inverse of 'x'
}
