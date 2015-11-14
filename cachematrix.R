## KW  November 12, 2015
## This pair of functions will take a matrix object, calculate the invert,
## store the inverted value, and return a message if the value called
## was a cached value if accessed via cacheSolve. This process is handled 
## through a series of internal functions that get and set values for the 
## matrix object.
## Usage example:
##   v <- cbind(c(1,2),c(2,1))  # Create matrix object value
##   v1 <- makeCacheMatrix(v)   # Set matrix object
##   v1$get()       # display matrix (invert does not yet exist)
##   cacheSolve(v1) # sets/displays invert; call 2x to see cache message
##   v1$getinv()    # displays invert; no message; uses cacheSolve value
##   v <- cbind(c(3,5),c(5,3))  # create new matrix object value
##   v1$set(v)      # set matrix object to new matrix value
##   cacheSolve(v1) # call 2x to see cache message displayed


## Function makeCacheMatrix will store and return a matrix and its inverted 
## form
makeCacheMatrix <- function(x = matrix()) {
        invertedMatrix <- NULL
        
        #internal function to set matrix to new value and clear inverted value
        set <- function(newMatrix) {
                x <<- newMatrix
                invertedMatrix <<- NULL
        }
        
        #internal function to get matrix value
        get <- function() x
        
        #internal function to solve (invert) the matrix
        setinv <- function(solve) invertedMatrix <<- solve
        
        #internal function to return the inverted matrix value
        getinv <- function() invertedMatrix
        
        #list to describe internal functions
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Function cacheSolve will calculate the invert of a matrix created by 
## makeCacheMatrix and store that value back into the makeCacheMatrix object.
## If this function is called more than one time for the same matrix, it will
## display a message indicating the inverted value was cached rather than 
## generated
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invertedMatrix <- x$getinv()
        
        #check if value exists and alert to cached status
        if (!is.null(invertedMatrix)) {
                message("getting cached data")
                return(invertedMatrix)
        }
        
        #get new matrix value
        freshData <- x$get()
        
        #solve (invert) matrix
        invertedMatrix <- solve(freshData, ...)
        
        #save inverted matrix value back to object
        x$setinv(invertedMatrix)
        
        #display inverted matrix value
        invertedMatrix
}
