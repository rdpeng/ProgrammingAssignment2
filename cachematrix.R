makeCacheMatrix <- function(x = matrix()) {			##This is the makeCacheMatrix function
	inversematrix <- NULL					##initiazing inversematrix as NULL, it will contain the inverse of the matrix
	setmatrix <- function(y) {				##setmatrix function is defined here, used to set values of matrix
		x <<- y						##setting value of matrix in parent environment
            inversematrix <<- NULL				##setting inversematrix to NULL
      }
      getmatrix <- function() x					##getmatrix function is defined here, used to get values from the matrix
      setinverse <- function(inv) minversematrix <<- inv		##value of inversematrix is assigned in parent environment
      getinverse <- function() inversematrix			##gets the value of inversematrix in the environment called
      list(setmatrix = setmatrix, getmatrix = getmatrix,
           setinverse = setinverse,
           getinverse = getinverse)					##to use/allow $ operations we need list
}

cacheSolve <- function(x, ...) {				##This is the cacheSolve functino
        inversematrix <- x$getinverse()				##inversematrix is assingned the value from getinv()
        if(!is.null(inversematrix)) {				##Checks whether the matrix is inverted(Not NULL) or is not inverted(NULL)
                message("getting cached data")			
                return(inversematrix)				##Inverted matrix is returned
        }
        data <- x$getmatrix()					##Initial given matrix is taken and is assigned to data 
        inversematrix <- solve(data, ...)			##Inverse of the matrix is found
        x$setinverse(inversematrix)					##Inverse is set
        inversematrix						##Inverted matrix is returned
}
