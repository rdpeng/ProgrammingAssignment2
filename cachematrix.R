## Put comments here that give an overall description of what your
## functions do
## My functions creates a special "matrix" object that can cache its inverse
## due to time consuming inversion process for a big matrixes
## and cacheSolve computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), 
## then the cacheSolve retrieve the inverse from the cache

makeCacheMatrix <- function(x = matrix()) {
  	m<-NULL
  	set<-function(y){
  		x<<-y
  		m<<-NULL
}
	get<-function() x 
	## returns the matrix "x" stored in the function above
	setmatrix<-function(solve) m<<- solve 
	## inverses the matrix stored in the main function
	getmatrix<-function() m
	list(set=set, get=get,
   		setmatrix=setmatrix,
   		getmatrix=getmatrix) 
   		## store the value of the input in a variable m
}


## Write a short comment describing this function
## My function computes the inverse of the special "matrix" returned by makeCacheMatrix
## function above and if its inverse was already calculated (and the matrix has not changed)
## than cacheSolve returns the inverse from the cache

cacheSolve <- function(x, ...) {
	m<-x$getmatrix() 
   	 if(!is.null(m)){ 
   	 ## if the calculation was already done it returns the cache data
      	message("getting cached data")
      	return(m)
    }
    matrix <- x$get() 
    ## gets the matrix stored with makeCacheMatrix
    m<-solve(matrix, ...) 
    ## for calculating the inverse
    x$setmatrix(m) 
    ## to store it in the object m
    m
}
