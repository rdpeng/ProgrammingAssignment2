## set the value of matrix
## get the value of matrix


makeCacheMatrix <- function(x = matrix()) {
	j<-NULL
	set<-function(y){
		x<<-y
		j<<-NULL
	}
	get<-function() x
	setinverse<-function(inverse) j<<-matrix
	getinverse<-function() j
	list(set=set,get=get,
	     setinverse=setinverse,getinverse=getinverse)

}


## set the value of inverse 
## get the value of inverse


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        j<- x$getinverse()
        if(!iss.null(j)) {
        	message("getting cached data")
        	return(j)
        	
        }
        mat<-x$get()
        j<- solve(mat,...)
        x$setinverse(j)
        j
}
