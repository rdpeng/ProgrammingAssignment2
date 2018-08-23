##The functions are to check the availability of requested inverse of matrix before performing the function, in order to aviod repeated calculations.  

##The function makeCacheMatrix is to create an object to cache the inverse of matrix.

makeCacheMatrix <- function(x = matrix()) {
        in <- NULL
	set <- function(y){
		x <<- y
		in <<-NULL
	}
	get <- function()x
	setinverse <- function(inverse) in <<-inverse
	getinverse <- function() in
	list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)

}


##The function cacheSolve is to calculate the inverse of matrix; if it has been calculated before, the function will retrieve the result from the cache.

cacheSolve <- function(x, ...) {
       in <- x$getinverse()
	if(!is.null(in)){
		message("getting cached data")
		return(in)
	}
	data <- x$get()
	in <- solve(data,...)
	x$setinverse(in)
	in

}
