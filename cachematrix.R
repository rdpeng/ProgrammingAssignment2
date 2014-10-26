## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##set as well as get the value of matrix and its inverse
makeCacheMatrix <- function(x = matrix()) {
inver<-NULL
set<-function(z){
x<<-z
inverse<<-NULL
}
get<-function()x
setinverse<-function(inverse) inver<<-inverse
getinverse<-function()inver
list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}
## Write a short comment describing this function
##returns the inverse of matrix. if its already computed, returns it from the cached data.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inver<-x$getinverse()
        if(!is.null(inver)){
        message("got cached data.")
        return(inver)
        }
        data<-x$get()
        inver<-solve(data)
        x$setinverse(inver)
        inver
}
