## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

        #it makes a special matrix from a given matrix
        #initialise cache matrix and assign the value NULL for initialisation
        cacheMatrix <- NULL
        
        #defining the method to set matrix
         setMatrix <- function(k) {
                 x <<- k
                 cacheMatrix <<- NULL
                 }
        #defining the method to get matrix
        getMatrix <- function() {
                
                x
                }
        #defining the method to set inverse of the matrix
        setInverse <- function(inverse) {
                cacheMatrix <<- inverse
                }
        #defining the method to get inverse of the matrix
        getInverse <- function() {
                cacheMatrix
                }
        #list the names of all methods
        list(setMatrix = setMatrix,
             getMatrix = getMatrix,
             setInverse = setInverse,
             getInverse = getInverse)
        
        
        
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        #returns the inverse of the special matrix given by above function
        
        #returns the inverse of the matrix
        cacheMatrix <- x$getInverse()
        
        #if content is not NULL, it returns the inverse
        
        if (!is.null(cacheMatrix)) {
                message("getting cache matrix")
                return(cacheMatrix)
                }
        
        else {
                #get the matrix
                data <- x$getMatrix()
                
                #find the inverse 
                cacheMatrix <- solve(data, ...)
                
                #set the inverse
                x$setInverse(cacheMatrix)
                
                return(cacheMatrix)
                }
  
        
}
