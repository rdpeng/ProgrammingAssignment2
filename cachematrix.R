## Put comments here that give an overall description of what your
## functions do
##create a "matrix" that can store a matrix that is the inverse of this "matrix",if the "matrix" 
##do not change, we can give the inverse of this "matrix" instead of calculation again. 
## Write a short comment describing this function
##makeCacheMatrix create the special "matrix"
makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL
  set<-function(y){
    x <<- y
    inv <<- NULL
  }
  get<-function() x
  setinverse<-function(inverse) inv <<-inverse
  getinverse<-function() inv
  list(set=set,get=get,setinv=setinverse,getinv=getinverse)
}


## Write a short comment describing this function
## calculate the inverse of the "matrix" the first time we meet it.If the matrix do not change
## we can give the inverse of it directly without doing the calculation again.
cacheSolve <- function(x, ...) {
  inv<-x$getinv()
  if(!is.null(inv)){
    message("get cached inverse value")
    inv
  }
  matrix<-x$get()
  inv<-solve(matrix,...)
  x$setinv(inv)
  inv     ## Return a matrix that is the inverse of 'x'
}
