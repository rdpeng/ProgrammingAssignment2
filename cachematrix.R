## makeCacheMatrix creates a Matrix object that can cache its inverse
## cacheSolve will compute and save the inverse
## variables are named with a for array, d for double

## the new Matrix object will contain 4 functions as a list
## to set and retrieve the Matrix itself and its inverse

makeCacheMatrix <- function(adMatrix = matrix()) {
	adInvMatrix<-NULL

	setMatrix<-function(y) {
		adMatrix<<-y
		adInvMatrix<<-NULL
	}
	getMatrix<-function() adMatrix
	getInv<-function() adInvMatrix
	setInv<-function(adY) adInvMatrix<<-adY
	list(	setMatrix=setMatrix,
		getMatrix=getMatrix,
		getInv=getInv,
		setInv=setInv)
}


## this function check if the inverse has been computed before solving for it

cacheSolve <- function(x, ...) {
	adInv <- x$getInv()
	if(is.null(adInv)){
		adMatrix<-x$getMatrix()
		adInv<-solve(adMatrix, ...)
		x$setInv(adInv)
	}
	return(adInv)
}
