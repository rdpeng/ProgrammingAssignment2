## makeCahce matrix has 4 functions for the following operations. 
## set function to set the value of the matrix
## get function to get the value from the matrix
## setinverse to set the value of inverse matrix
## getinverse to get the value of inverse matrix
makeCacheMatrix <- function(x = matrix()) {
    cachedata<-NULL
    set<-function(y){
        x<<-y
        cachedata<<-NULL
    }
    get<-function()x
    setinverse<-function(inverse) cachedata<<-inverse
    getinverse<-function() cachedata
    list(set=set,
         get=get,
         setinverse=setinverse,
         getinverse=getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
    cachedata<-x$getinverse()
## if already in cache, then return the cache    
    if(!is.null(cachedata)){
        message ("getting chaced data...")
        return(cachedata)
    }
    
## if not in chance, generate matrix inverse
    message("Generating the matrix inverse freshely...")
    data<-x$get()
    cachedata<-solve(data,...)
    x$setinverse(cachedata)
}
