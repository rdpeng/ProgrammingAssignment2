## Put comments here that give an overall description of what your
## functions do

## first function which creates a special matrix that caches its inverse

makeCacheMatrix <- function(myMatrix = matrix()) {  #first function which creates a special matrix that caches its inverse
        inverseMatrix <- NULL                      #creates an empty vector to store inverse matrix
        setMatrixValue <- function(xxx) {          #sets the value of myMatrix
                myMatrix <<- xxx
                inverseMatrix <<- NULL
        }
        getMatrixValue <- function() myMatrix                                   #gets the value of myMatrix
        setInverseValue <- function(inverse)inverseMatrix<<-inverse             #sets the value of inverse
        getInverseValue <- function() {inverseMatrix}                           #gets the value of inverse
        list(setMatrixValue = setMatrixValue, getMatrixValue = getMatrixValue,  #creates a list to set and store values
             setInverseValue = setInverseValue,                                 # of matrix and inverse
             getInverseValue = getInverseValue)
}


## second function computes inverse of the special matrix
## if inverse is already calculated, it will retrieve inverse from the cache

cacheSolve <- function(myMatrix , ...) {                 #second function computes inverse of the special matrix
        inverseMatrix <- myMatrix$getInverseValue()
        if(!is.null(inverseMatrix)) {                    #if inverse already calculated, it will retrieve inverse from cache
                message("Inverse already calculated, getting cached data")
                return(inverseMatrix)
        }
        myData <- myMatrix$getMatrixValue()
        inverseMatrix <- solve(myData, ...)               #computes inverse of myMatrix
        myMatrix$setInverseValue(inverseMatrix)
        inverseMatrix                                     #returns a matrix that is the inverse of myMatrix
}