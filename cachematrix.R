## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix creates a special matrix including a list of functions.
## These functions do the following:
## 1. set the matrix
## 2. get the matrix
## 3. set the inverse of the matrix
## 4. get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
	##i is the inverse of the matrix. Set i as NULL	
	i<-NULL
	set<-function(y){
	x<<-y
	i<<-NULL
	}
	get<-function()x
	setinverse<-function(inverse)i<<-inverse
	getinverse<-function()i
	list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


## cacheSolve does the following: It calculates the inverse of the matrix created wth the above function
## It first checks to see if the inverse of the matrix has been calculated.
## If the inverse has been calculated, then it will get the inverse from the cache and skip the calculation. 
## Otherwise it will calculate the inverse of the matrix. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'. Use "$getinverse" to call the getinverse function in makeCacheMatrix
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
