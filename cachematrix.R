## Put comments here that give an overall description of what your
## functions do
## The functions cache the inverse of a matrix instead of computing it repeatedly to avoid costly computations.
## Write a short comment describing this function
## The makeCacheMatrix creates an object in the parent environment which can cache the inverse of the matrix. The function cacheSolve can retrieve from the function makeCacheMatrix, the inverse of the matrix if it has been calculated before.

makeCacheMatrix <- function(x = matrix()) {
	m<-NULL
	set<-function(y){
		x<<-y
		m<<-NULL
	}
	get<-function() x
	setmatrix<- function(inverse) m<<-inverse
	getmatrix<-function() m
	list(set=set, get=get, setmatrix=setmatrix, getmatrix=getmatrix)
}


## Write a short comment describing this function
## The function retrieves the matrix which is computed in makeCacheMatrix.
## It checks whether its inverse has been already calculated. If it is, it retrieves the cached matrix from the makeCacheMatrix. If it is not, it calculates it.
cacheSolve <- function(x, ...) {
	m<-x$getmatrix()
	if(!is.null(m)){
			message("getting cached data")
			return(m)
	}
	
        ## Return a matrix that is the inverse of 'x'
    data<-x$get()
	m<-solve(data)
	x$setmatrix(m)
	m
        
}
