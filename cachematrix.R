## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

##Set up inverse property
i <- NULL

##Set up the matrix
set <-function(matrix) {m <<- matrix
	i<<-NULL}
}

##Get the matrix
getmatrix<-function(){
	##Return matrix
	m
}
##Set up matrix inverse
setinverse <-function(inverse) {i<<-inverse}

##Get matrix inverse
getinverse <-function() {
	##return inverse property
	i
}

##return list of methods
list(set=set, getmatrix=getmatrix, setinverse=setinverse, getinverse=getinverse)

##compute cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.
## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
} m<-x$getinverse()

## If the inverse has already been calculated (and the matrix has not changed), then retrieve the inverse from the cache.
if(!is.null(m)) {message("getting cached data") return (m)}

##get matrix from object

data<-x$getmatrix()

##calculate inverse using matrix multiplication

m<-solve(data)%*%data

##set inverse to object

x$setinverse(m)

##return matrix

m
