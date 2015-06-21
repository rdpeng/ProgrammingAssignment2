## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
	
	setMat <- function(y){
		x <<- y
		i <<- NULL
	}
	
	getMat <- function()
		x

	setinver <- function(z){
		i <<- z
	}

	getinver <- function() i

	list(setMat = setMat, getMat = getMat, setinver= setinver, getinver= getinver)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinver()

        if(!is.null(inv)) {
                message("it had been calculated!")
                return(inv)
        }

	  data <- x$getMat()
	  
        i <- solve(data)
        x$setinver(i)
        i
}
