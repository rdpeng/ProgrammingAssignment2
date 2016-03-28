## Put comments here that give an overall description of what your
## functions do
##cache the inverse of a matrix, because matrix inversion might be time-consuming
##when computing the results. In the following functions, I first create a
##special matrix taht can cache its inverse and then create another function
##that can computes the inverse of the matrix. If the inverse has been already
##computed the second function can retrieve the inverse from the cache. 

## Write a short comment describing this function
## makeCacheMatrix is the first function mentioned above that can set, get the matrix and set and get the inverse of this matrix
makeCacheMatrix <- function(m = matrix()) {
	inver<-NULL
	set<-function(y){
		m<<-y
		inver<<-NULL
	}
	get<-function(){
		m
	}
	setinverse<-function(inv) {
		inver<<-inv
	}
	getinverse<-function(){
	inver	
	} 
	list(set=set,get=get,setinverse=setinverse, getinverse=getinverse)
}


## Write a short comment describing this function
## This second function can return the inverse of a matrix
##if the inverse has already been computed, the function will find an inverse matrix from the cache. If not,it will computes the result
cacheSolve <- function(m, ...) {
       inv<-m$getinverse()
	if(!is.null(inv)){
		message("getting cached data")
		return(inv)
	}
	data<-m$get()
	inv<-solve(data,...)
	m$setinverse(inv)
	inv ## Return a matrix that is the inverse of 'm'

}
