## makeCacheMatrix creates a matrix object that can cache its inverse
## cacheSolve computes the inverse of the matrix returned by makeCacheMatrix
## If the inverse is calculated, cacheSolve retrives the value from the cache


## set:set the value of a matrix
## get:get the value of a matrix
## setinverse:set the inverse of a matrix
## getinverse:get the inverse of a matrix


makeCacheMatrix <- function(x = matrix()) {
m<-NULL
set<-function(y){
x<<-y
m<<-NULL
}
get<-function() x
setinverse<-function(solve) m<<-solve
getinverse<-function() m
list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


## If the inverse is calculated in makeCacheMatrix, 
## cacheSolve retrives the value from the cache

cacheSolve <- function(x, ...) {
cacheSolve <- function(x, ...) {
m<-x$getinverse()
if(!is.null(m)){
message("getting cached data")
return(m)
}
data<-x$get()
m<-solve(data,...)
x$setinverse(m)
m

}
