## The objective is the matrix inversion
## This function creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x=matrix()){
	m <- NULL
	set <- function(y){
		x <<- y
		m <<- NULL
	}
	get <- function()x
	setinverse <- function(inverse)m <<- inverse
	getinverse <- function()m
	list(set=set,get=get,setinverse=setinverse ,getinverse=getinverse)
}

## This function computes the inverse of the matrix returned by the above function

cacheSolve <- function(x,...){
	m <- x$getinverse() 	#query the x matrix's cache         
	if(!is.null(m)){ 		#if there is a cache
		message("getting cached data")
		return(m)  		#just return the cache, no computation needed
	}
	data <- x$get() 		#if there's no cache
	m <- solve(data,...) 	#we actually compute them here
	x$setinverse(m) 		#save the result back to x's cache
	m 				#return the result
}
