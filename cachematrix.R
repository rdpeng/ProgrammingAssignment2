## this function has two variables :x and m
## x is the matrix that is supposed be dealed
## m is used to store the results
makeCacheMatrix <- function(x = matrix()) {
m<-NULL
set<-function(y){
	x<<-y
	m<<-NULL
}
## return x 
get<-function()x
setinverse<-function(inverse)  m<<-inverse
getinverse<-function()m
list(set=set,
     get=get,
     setinverse=getinverse,
     getinverse=getinverse)
}


## the output variable must be a matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## x$getinverse calls the function of getinverse in makeCacheMatrix
m<-x$getinverse()
if(!is.null(m)){
	    ##if m isnot null, that means the inverse of the matrix has been calculated.
	    ##return the result
	    message("getting cached data")
	    return(m)
}
     ##if the function perform to now, that means the inverse of the matrix has not been calculated.   
}
data<-x$get()
##calculate the inverse 
m<-inverse(data,...)
x$setinverse(m)
m
}