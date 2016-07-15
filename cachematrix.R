# Caching the Inverse of a Matrix
# Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix rather than compute it repeatedly.
# The below function is used to create a special object that stores a matrix and caches its inverse.

makeCacheMatrix <- function(x = matrix()) {
	inv<-NULL
	set<-function(y){
		x<-y
		inv<-NULL
	}
	get<-function() x
	setinv<-function(inverse) inv<-inverse
	getinv<-function() inv
	list(set=set,
		 get=get,
		 setinv=setinv,
		 getinv=getinv)

}


# The cacheSolve function computes inverse of special matrix created by above makeCacheMatrix. If inverse has already been calcualted (and the matrix has not been modified), then it should retrieve the inservse from cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv<-x$getinv()
        if(!is.null(inv)){
        		message('get cached')
        		return(inv)
        }
        matrix<-x$get()
        inv<-solve(matrix,...)
        x$setinv(inv)
        inv
}
