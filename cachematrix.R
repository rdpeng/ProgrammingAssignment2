## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function: this function will cache the inverse matrix for any invertible matrix

makeCacheMatrix <- function(x = matrix()) {
inv<-NULL
    set<-function(y){
        x<<-y
        inv<<-NULL
    }
    get<-function()x
    setinverse<-function(inverse) inv<<-inverse
    getinverse<-function() inv
    list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


## Write a short comment describing this function: this function will firstl try to pull out cached result, if not cached, then calculate the inverse matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv<-x$getinverse()
    if(!is.null(inv)){
        message("getting cached data")
        return(inv)
    }
    matrixdata<-x$get()
    inv<-solve(matrixdata,...)
    x$setinverse(inv)
    inv
        
    
}
