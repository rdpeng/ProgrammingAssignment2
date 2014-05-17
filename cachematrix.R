## Programming Assignment 2
## Creates a  matrix that caches its inverse
## Inverse of the matrix returned 
## If the matrix is already calculated retrieve the inverse from the cache

## Set the value of matrix 
## Get the value of matrix
## Set the value of inverse
## Get the value of inverse

makeCacheMatrix<-function (x=matrix())
{

	## set the value of matrix

	inverse<-NULL


	set<-function(y)
	{
		x<<- y
		inverse<<- NULL
	}

	## Get value of matrix

	get<- function()x

	## Set inverse matrix

	set.inverse<- function(solve) inverse<<-solve
	get.inverse<- function() inverse

	
	## Get the inverse of matrix


	list(
  		set=set,
  		get=get,
  		set.inverse=set.inverse,
  		get.inverse=get.inverse)
}


cacheSolve<-function(x, ...)
{
	## Return inverse matrix of x
	## Get the inverse matrix
	inverse<-x$get.inverse()
	if(!is.null(inverse))
	{
		message("Getting the cached inverse data")
		return(inverse)
	}
	##if not : get the inverse 
	{
	 data<-x$get()
	 inverse<-solve(data, ...)
	## Set inverse of the matrix
	 x$setinverse(inverse)
	 inverse
}
