## Two functions that are defined here allow to cache inverted matrix
## thus saving resources if invertion is done multiple times.

## makeCacheMatrix function creates list of functions that store and
## retrieve cached source matrix or inverse matrix.
## User first needs to create list of functions, e.g.
## mxlist<-makeCacheMatrix(mymatrix)

makeCacheMatrix <- function(x = matrix()) {
	i<-NULL
	set<-function(y){
		x<<-y
		i<<-NULL
	}
	get<-function() x
	setinverse<-function(inv) i<<-inv
	getinverse<-function() i
	list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## cacheSolve function will return inverse of matrix.
## If inverse matrix has previously been calculated, the cached value is
## returned. If inverse matrix has not been calculated, cacheSolve will
## calculate it and cache for later use. As the argument user has to give
## the list (e.g. mxlist) that had been defined previously using
## makeCacheMatrix function. 

cacheSolve <- function(x, ...) {
	i<-x$getinverse()
	if(!is.null(i)){
		message("getting already cached data")
		return(i)
	}
      originMatrix<-x$get()
	i<-solve(originMatrix)
	x$setinverse(i)
	i
}
