## The two functions together create a inverse matrix and enable the inverse of
## the matrix availble in the cache environment

## This function creates and returns list of functions used in the 2nd function to get/set the inverse matrix in the cache

makeCacheMatrix <- function(x = matrix()) {
cache<-NULL
set <-function (matrix){
x<<-matrix
cache<<-NULL
}
##setting the invert matrix and storing in cache
## then get the inverted matrix from cache
get<- function(){x}

setInverse<-function(inverse){
cache<<-inverse}
getInverse<- function()
{
cache}
list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}
## cacheSolve calcluates the inverse of the matrix created in the above function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
cache<-x$getInverse()
if(!is.null(cache)){
message("getting cache data")
return(cache)}
data<-x$get()
cache<-solve(data)
x$setInverse(cache)
cache
}
