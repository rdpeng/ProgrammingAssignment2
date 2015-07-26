## Put comments here that give an overall description of what your
## functions do

## In this function objects created, to store inverse of matrix

makeCacheMatrix <- function(x = matrix()) {

        ## Initialize the inverse matrix as NULL
        inverse <- NULL
        
        ## Method to set the matrix
        set <- function(y){
                matrix <<- y
                inverse <<- NULL
        }
        
        ## Method to get the matrix
        get <- function(){
                ## return matrix
                matrix
        }
        
        ## Method to set the inverse of the matrix
        setInverse <- function(inverseMatrix) {
                ## storing inverse matrix
                inverse <<- inverseMatrix
        }
        
        ## Method to get the inverse of the matrix
        getInverse <- function() {
                ## returns the inverse matrix
                inverse
        }
        
        ## Returns the list of methods
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## This function computes inverse of matrix, if it does not exist in cache. Otherwise it retrives 
## inverse of matrix from cache

cacheSolve <- function(x, ...) {
      
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getInverse()
        
        ## returns inverse matrix, if inverse has already been calculated
        if(!is.null(inverse)) {
                message("Getting cached inverse data")
                return(inverse)
        }
        
        ## If the inverse wasn't yet been calculated 

        ## Get matrix from the data
        matrixData <- x$get()
        
        ## calculate inverse by using matrix multiplication
        invMatrix <- solve(matrixData) %*% matrixData
        
        ## store the inverse
        x$setInverse(invMatrix)
        
        ## returning a matrix that is the inverse of 'x'
        invMatrix
}
