## This function cache solve returns the inverse of a matrix created withinthe makecacheMatrix function.
If cache inverse is available, cache solves retrives it, and if not, it computes caches and returns it. 

## Creates a special "matrix"object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
i<-NULL
set<-function(y){
	x<<-y
	i<<-NULL
}
get<- function() x
setinverse<-function(inverse) i<<- inverse
getinverse<-function() i
list(set=set, get=get, setinverse=setinverse, getinverse=getinverse )

}

## Computes the inverse. If the inverse has already been calculated and the matrix has not changes, then the cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
       i<-x$getinverse()
       if(!is.null(i)){
       	message("getting cached data")
       	return(i)
       }
       data<-x$get()
       i<-solve(data,...)
       x$setinverse(i)
       i
}
