## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## makeCacheMatrix() returns a list of functions to set and fetch
## matrix and its inverse

makeCacheMatrix <- function(x = matrix()) 
{
	invert<-NULL
	set<-function(y)
	{
		x<<-y          ##assigning values in cache(different environment - outside the scope of this environment)
		invert<<-NULL  ##assigning values in cache(different environment)
	}
	  ##Using a different environment helps increase the scope of variables\
	  ##can be accessed in other functions
	get<-function() x
	  ##access the value of matrix
	setinverse<-function(inverse) invert<<-inverse
	  ##saves the invert of a matrix in cache
	getinverse<-function() invert
	list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)	
}


## Write a short comment describing this function
##cacheSolve() function takes the value of matrix from the cache
## and returns the inverse of that matrix
## in case the result is already computed, the function
##will give the already computed result saved in the cache w
## without spending time on repeating the computation

cacheSolve <- function(x, ...)
{
	invert<-x$getinverse()
	if(!is.null(invert))
	{
		print("getting cached data")
		return(invert)	##if the computation is already done on a particular invertible matrix
	}
	data<-x$get()
	invert<-solve(data, ...)
	  ##solve is an inbuilt function to find the inverse of a matrix
	x$setinverse(invert)
	  ##sets and saves the inverse for further application and 
	  ##low run-time processing
	return(invert)
        ## Return a matrix that is the inverse of 'x'
}
