## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## The function below creates a special matrix named "makeCachematrix" this object
## cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  l<-NULL
  set <- function(y){
   x<<-y
   l<<- NULL
  }
  get<-function()x
  setinverse<-function(inverse) l <<- inverse
  getinverse<-function()l
  list(set=set,
       get=get,
       setinverse=setinverse,
       getinverse=getinverse)
}


## Write a short comment describing this function
## The function below computes the inverse "makeCachematrix" matrix.
## However if the inverse has already been calculated then it would 
## reterieve the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  l<-x$getinverse()
  if(!is.null(l)){
    message("getting cached data")
    return(l)
  }
  mat<-x$get()
  l<-solve(mat,...)
x$setinverse(l)
l
}
