## Put comments here that give an overall description of what your
## functions do
##Here are two functions that have been used
##makeCacheMatrix and cacheSolve that cache the inverse of the matrix
## Write a short comment describing this function
##makeCacheMatrix creates special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {

}
makeCacheMatrix<-function(x=matrix()) {
i<<-NULL
set<-function(y){
  x<<-y
 i<<-NULL 
}  
get<-function() x 
 setinverse<-function(inverse)i<<-inverse 
getinverse<-function()i
list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}

## Write a short comment describing this function
##cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix
##above.If the inverse has already been calculated,then cacheSolve should retrieve
##the inverse from the cache
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
}
cacheSolve<-function(x, ...){
i<-x$getinverse()
if(!is.null(i)) {
message("getting cached matrix")  
return(i)  
}  
matrix<-x$get()
i<-solve(matrix, ...)
x$setinverse(i)
i
}


## testing the function
x<-makeCacheMatrix()
x$set(matrix(1:6,2,3))
x$get()
x$setinverse()
x$getinverse()
