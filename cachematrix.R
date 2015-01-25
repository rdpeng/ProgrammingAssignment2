## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL ##Gets the inverse matrix 
	n <- NULL ##Gets current matrix 
	set1 <- function(y){
	 	x<<-n
		m<<-NULL
	}
	get1 <- funtion()x ##gets matrix data
	set2 <- function(s) m <<- s ##sets the computed matrix to m
	get2 <- function() m ##gets the cached inverse matrix 
	##Returns the list
	    list(set1=set1, get1=get1, set2=set2, get2=get2)

}


## cacheSolve: This function computes the inverse of the special "matrix" 
##returned by makeCacheMatrix above. If the inverse has already been 
##calculated (and the matrix has not changed), then the cachesolve
##should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
            m <- x$get2()
    if(!is.null(m)){ ## See if Cached data is found, if found then do these operations
        return(m)
    }
    else { ## If Cached data is not found, then do these operations
        w <- x$get() ## gets matrix from x
        m <- solve(w) ## finds inverse matrix from x
        x$set2(m) ## assigns the inverse matrix to x
        return(m) 
	}
    
}
