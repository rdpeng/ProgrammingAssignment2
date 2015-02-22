# makeCacheMatrix function calculates the inverse of a matrix 'x' using the in-built function 'solve'

makeCacheMatrix <- function(x = matrix()) { 
  
  # Function variable matrixinverse holds the value for the inverse of a matrix x. Initialise the value to NULL for this variable t
  matrixinverse <- NULL                     
  
 # Set function assigns a value to matrix 'x'
  set <- function(y) {                      
    x <<- y
    matrixinverse <<- NULL              
  }
 
 # Get function obtains the value for matrix 'x'
   get <- function() x                           

 #Setinverse function calculates the inverse of a matrix 'x' using the in built function 'solve'
  setinverse <- function(solve) matrixinverse <<- solve 
  
 #Getinverse function obtains the value for the inverse of the matrix 'x'
  getinverse <- function() matrixinverse  
 
       
  list(set = set, get = get,                    
       setinverse = setinverse,
       getinverse = getinverse)
}


#Use cacheSolve to obtain the cached value for the inverse of a  matrix
cacheSolve<- function(x, ...) {                 
  matrixinverse <- x$getinverse()
  
  #Obtain the value for the inverse matrix if this exists in the cached memory
  if(!is.null(matrixinverse)) {                 
    message("getting cached data - Inverse of the matrix")
    return(matrixinverse)
  }
  #Calculate the value for the inverse of a matrix if the value does not exist in the cached memory
  data <- x$get()                               
  matrixinverse <- solve(data, ...)
  x$setinverse(matrixinverse)
  matrixinverse
}
 
# Call and extract the 'makeCacheMatrix' function
x<- matrix(runif(16), nrow= 4, ncol =4)
makeCacheMatrix(x)

#Call and extract the 'cacheSolve' function
cacheSolve(makeCacheMatrix(x))
