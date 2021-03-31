#### Programming Assignment 2 ####

## The functions presented here can make a matrix that can have an be used to
## calculate its inverse value. It has various functions within them that aid
## the process of making the matrix, making sure it can be inverted, storing 
## the data and then providing an inverse matrix at the end. 



# Make a function that can create a matrix object and also 
# it can cache the inverse of matrix
makeCacheMatrix <- function(x = matrix()) {
      
      #States the inverse property to apply to the matrix
      inv <- NULL
      
      #Sets the matrix
      setmatrix <- function(y){
            x <<- y
            inv <<- NULL
            }
      
      #Gets the matrix value and calls it
      getmatrix <- function() {
            x
            }

            #Set the inverse of the matrix and value is stored
      setInverse <- function(inverse){
                  inv <<- inverse
            } 
            
      #Gets the value of the inverse of the matrix
      getInverse <- function() {
            #Return inverse property of the matrix
            inv
      }
      
      #The list houses the four functions created
      list(setmatrix = setmatrix, getmatrix = getmatrix, 
           setInverse = setInverse, 
           getInverse = getInverse)
}


#Checks if the matrix created and inverse, and if it hasn't, it will create one

cacheSolve <- function(x, ...) {
      
      #If the inverse has been calculated using makeCacheMatrix(), it will
      #be stored here
      m <- x$getInverse()
      
      #checks to see if the function makeCacheMatrix has been run      
      if( !is.null(m) ) {
            message("getting cached data")
            return(m)
      }
      
      #gets the matrix from the object
      data <- x$getmatrix()
      
      #calculates the inverse (matrix multiplication)
      m <- solve(data)
      
      #sets the inverse
      x$setInverse(m)
      
      #Return the matrix
      m
}

