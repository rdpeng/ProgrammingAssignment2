# This function creates a list with functions to
# set the matrix
# get the matrix
# set the inverse of the matrix
# get the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
        
  #Inicialize variable for the inverse matrix
  inverseMatrix<-NULL
  
  #set the matrix
  set <- function(y){
    x<<-y
    inverseMatrix<-NULL
  }
  #get the matrix
  get <- function() {
    x
  }
  #to store inverse matrix in the variable -> inverseMatrix
  setinverse <- function(y) {
    inverseMatrix <<- y
  }
  #return value of inverse matrix
  getinverse <- function() {
    inverseMatrix
  }
  # make a list with functions
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}



#Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
	#searching in cache
	inverse <- x$getinverse()
	
	#look for if inverse matrix is stored in the cache
	if(!is.null(inverse)){
		message("one moment please ... searching in cached values")
		return(inverse)
	}
	
	#There isn´t inverse matrix stored in the cache
	#Get the matrix paramater
	matrix<-x$get()

	#For this assignment, assume that the matrix supplied is always invertible.
	#obtain de inverse matrix using function solve
	inverse<-solve(matrix, ...)
	
	#to store inverse matrix in the cache
	x$setinverse(inverse)
	
	inverse
	
}
