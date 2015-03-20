## course ID: rprog-012
##
## Notes from the instructor:
## Computing the inverse of a square matrix can be done 
## with the solve function in R. For example, if X is a 
## square invertible matrix, then solve(X) returns its inverse.
## assume that the matrix supplied is always invertible.
## Create a special matrix object and can cache its inverse,
## applying "Lexical Scoping" in R environment.

##__________________________________________________________________________
## mackeCacheMatrix Creates a "matrix" object that can cache its inverse
##__________________________________________________________________________
##
makeCacheMatrix <- function(x = matrix()) {
                                                             
  inverseMatrix <- NULL                                     # Initialize the parent environment to be empty (NULL)
##                                                         
##
##                                              
  set <- function(y) {                                      # <- assign to the current environment
    x <<- y                                                 # <<- rebind to the parent of the current environment.
    inverseMatrix <<- NULL                                  # Set environment to parent (inverseMatrix == NULL) for "makeCacheMatrix" 
                                                            
  }
  
  get <- function() x                                       # Create a function called 'get' from makeCacheMatrix, assign it matrix x
  
  setInverse <- function(solve) inverseMatrix <<- solve     # Take the value of "solve" and set it to the environment of inversematrix
  
  getInverse <- function() inverserMatrix                   # Return the value for 'inverseMatrix' from its environment
  
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)                             # Retrive/push the inverse from/to cache
                                                            

}
##_________________________________________________________
## cacheSolve: This function computes the inverse of the 
## matrix returned by makeCacheMatrix above if the inverse 
## has already been calculated. If not, it computes it.
##_________________________________________________________
##
cacheSolve <- function(x, ...) {

  inverseMatrix <- x$getInverse()                           # Get the inverseMatrix from the inverseMatrix environment
  if(!is.null(inverseMatrix)) {                             # Return inverse of the matrix   
    message("getting cached data")                          # display the corresponding message
    return(inverserMatrix)                                  # return the inverse of the matrix
  }
  data <- x$get()                                           # Assign "x" matrix to data if "x" has 
                                                            # not been evaluated before (inverseMatrix == NULL) 
  inverseMatrix <- solve(data, ...)                         # Calulate the inverse of the matrix using "solve" function 
                                                            # in the current environment
  x$setInverse(inverseMatrix)                               # Assign the inverse to the "x" environment via setInverse
  inverseMatrix                                             # Display the inverse

}