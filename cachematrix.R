## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
inverse<-NULL
set<-function(y){
 x<<-y
 inverse<<-NULL

}
get<-function()x
setinverse<-function(inv){
inverse<<-inv

}
getinverse<-function()inverse
list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}
## Write a short comment describing this function

cacheSolve <- function(x, ...) {
inverse<-x$getinverse
   if(!is.null(inverse)){
   message("getting cached data")
   return (inverse)}
   mat<-x$get
   inverse<-solve(mat)
   x$setinverse(inverse)
   inverse
   
        ## Return a matrix that is the inverse of 'x'
}
