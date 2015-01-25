## Below is a pair of functions that cache the inverse of a matrix
## The first function, makeCacheMatrix() creates a special "matrix", which is really a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse matrix
## 4. get the value of the inverse matrix

## makeCacheMatrix() -  This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        ## inverse matrix initialization
        iMatrix <- NULL
		
	## set the value of the matrix
        set <- function(y) {
                x <<- y
                iMatrix <<- NULL
        }
		
	## get the value of the matrix
        get <- function() x
		
        setMatrix <- function(inverse) iMatrix <<- inverse	
        getMatrix <- function() iMatrix
        list(set = set, get = get, setMatrix = setMatrix, getMatrix = getMatrix)
}


## cacheSolve() -  This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cacheSolve should retrieve the 
## inverse from the cache.

cacheSolve <- function(x, ...) {
 
        iM <- x$getMatrix()
		
	## check if the inverse matrix has already been calculated
        if(!is.null(iM)) {
                ## retrieve the inverse from the cache
                message("getting cached data")
                return(iM)
        }
	## if not, get the matrix into 'data' variable
        data <- x$get()
        iM <- solve(data, ...)
	## cache the inverse matrix
        x$setMatrix(iM)
	## Return a matrix that is the inverse of 'x'
        iM
}
