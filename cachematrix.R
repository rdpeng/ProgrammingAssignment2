## This function creates object that can cache matrix inverse.

makeCacheMatrix <- function(x = matrix()) {
        xMat<-makeCacheMatrix(x)
        parent.env(xMat$getenv())$m
         environment(xMat$getmean)
        m<-NULL
        evn <- environment()
                y<-NULL 
}

setmatrix<-function(y){  
	x<<-y  
	m<<-NULL
	}
  
getmatrix<-function() x  
setinverse<-function(solve) m<<- solve  
getinverse<-function() m  
getenv<- function() environment()

list (setmatrix=setmatrix, getmatrix = getmatrix,   
setinverse = setinverse,
getinverse = getinverse,
getenv = getenv)

}
##This function computes the inverse of the matrixreturned by function above. 
##cacheSolve retrieves the inverse from the cache.

cacheSolve <- function(xMat= m(), ...) {
	m <- xMat$getinverse() 
	if(!is.null(m)){ 
		if(xMat$setmatrix() == xMat$getmatrix()) { 
    	message("getting cached data")
    	matrix<-xMat$get()
    	m<-solve(matrix, ...)
    	xMat$setmatrix(m)
    	return(m) 
    	}
    	
    	y <- xMat$getmatrix() 
    	xMat$setmatrix(y)
    	m <- solve(y, ...) 
    	xMat$setinverse(m) 
    	m 
    	}
    	
}
