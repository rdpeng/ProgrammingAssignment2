## Create a pair of functions to cache the inverse of a matrix

## Thin function creates a special "matrix" object that can cache its inverse 
makeCacheMatrix <- function(x = matrix()) {
    i<-NULL
    set<-function(y){
        x<<-y
 	i<<-NULL
    }
    get<-function() x
    setinv<-function(inverse) i<<-inverse
    getinv=function() i
    list(set=set, get=get, setinv=setinv, getinv=getinv)
}
 

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve
##the inverse from the cache.Write a short comment describing this function
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
 	i<-x$getinv()
 	if(!is.null(i)){
 		message("getting cache data")
 		print (i)
 }
 data<-x$get()
 i<-solve(data,...)
 x$setinv(i)
 print(i)
}
