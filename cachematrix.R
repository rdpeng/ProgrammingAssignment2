## Put comments here that give an overall description of what your
## functions do
## my function creates a special "matrix" object that can cache its inverse
## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  	m<-NULL
  	set<-function(y){
  		x<<-y
  		m<<-NULL
}
	get<-function() x
	setmatrix<-function(solve) m<<- solve
	getmatrix<-function() m
	list(set=set, get=get,
   		setmatrix=setmatrix,
   		getmatrix=getmatrix)
}


## Write a short comment describing this function
## computes the inverse of the special "matrix" returned by makeCacheMatrix above

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	m<-x$getmatrix()
   	 if(!is.null(m)){
      	message("getting cached data")
      	return(m)
    }
    matrix <- x$get()
    m<-solve(matrix, ...)
    x$setmatrix(m)
    m
}
