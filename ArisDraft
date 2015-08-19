## Put comments here that give an overall description of what your
## functions do

#The below functions calculate the inverse of a matrix and caches
#the results for easy access and and to reduce computing time.

#To run this program:
# YourMatrixName <-makeCacheMatrix(matrix(1:16,8,8)) ##your choice
#of parameters. Please keep in mind that invertible matrices are 
#need to be SQUARED, i.e. 3x3, 4x4, etc.
#MatrixNameInverse <- cacheSolve(YourMatrixName)
#YourMatrixName$get() %*% YourMatrixName should return an identity 
#matrix
#Running MatrixNameInverse <- cacheSolve(YourMatrixName) should return the stored/cached data.

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
#This function creates a matrix according to the user 
#specifications using the "get" and "set" methods.

makeCacheMatrix <- function(x = matrix()) {
	m <-NULL
	set<-function(y){
	x <<-y
	m <<-NULL
}
	get<-function() x
	setmatrix <-function(solve) m<<-solve
	getmatrix <-function() m
	list(set = set, get = get,
		setmatrix = setmatrix,
		getmatrix = getmatrix)
}

#The below funtion solves to find the inverse of a matrix
#Then, it checks to see if the inverse has been calculated. If it
#isn't cached, it calculates it

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
	
	m<-x$getmatrix()
	if(!is.null(m)){
		message("getting cached data")
		return(m)
	}
	data <-x$get()
	m <-solve(data, ...)
	x$setmatrix(m){
		message("getting cached data")
		return(m)
	}
	data <-x$get()
	m <- solve(data, ...)
	x$setmatrix(m)
	m
        ## Return a matrix that is the inverse of 'x'
}
