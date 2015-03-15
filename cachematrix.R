## Instruction:
##
## Given the sample "vector" functions provided by instructor,
## This assignment is to create a special matrix object and can cache its inverse,
## applying "Lexical Scoping" in R environment.

##--------------------------------------------------------------------------
## Creates a special "matrix" object that can cache its inverse
##--------------------------------------------------------------------------
makeCacheMatrix <- function(x = matrix()) {
                                                            # Set its parent environment to be the empty environment (NULL). 
  inverseMatrix <- NULL                                     # This ensures you don't accidentally inherit objects from somewhere else
                                                            # Every environment has a parent; The parent is used to implement lexical scoping;

                                              
  set <- function(y) {                                      # <- always creates a binding in the current environment
    x <<- y                                                 # <<- rebinds an existing name in a parent of the current environment.
    inverseMatrix <<- NULL                                  # environment set to parent (inverseMatrix == NULL) for makeCacheMatrix, 
                                                            # so the solve (inverse) can be calculated and stored in "inverseMatrix"
  }
  
  get <- function() x                                       # Creates 'get' function from makeCacheMatrix and assign matrix "x"
  
  setInverse <- function(solve) inverseMatrix <<- solve     # Takes the value of "solve" and sets it to the inverseMatrix defined environment
  
  getInverse <- function() inverserMatrix                   # Returns the value of "inverseMatrix" from the inverseMatrix environment
  
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)                             # List of 4 named elements - set, get, setInverse, getInverse
                                                            # Each element is a function defined in environment from function(y) for makeCacheMatrix

}

##--------------------------------------------------------------------------
## Return a matrix that is the inverse of 'x'
##--------------------------------------------------------------------------
cacheSolve <- function(x, ...) {

  inverseMatrix <- x$getInverse()                           # Gets the inverseMatrix values from the defined inverseMatrix environment
  if(!is.null(inverseMatrix)) {                             # Returns inverse of the matrix and its   
    message("getting cached data")                          # corresponding message since it has
    return(inverserMatrix)                                  # already calculated (value "cache") in the environment
  }
  data <- x$get()                                           # Assigns "x" matrix to data if "x" has not been evaluated before (inverseMatrix == NULL) 
  inverseMatrix <- solve(data, ...)                         # Calulates the inverse of the matrix using "solve" function for the data variable in the current environment
  x$setInverse(inverseMatrix)                               # Assigns the calculated inverse to the "x" environment via setInverse
  inverseMatrix                                             # Displays the calculated inverse matrix

}


##==========================================================================
## Predefined "vector" sample functions below this line are provided by 
## class instructor
##==========================================================================
##
##--------------------------------------------------------------------------
## Creates a special "vector" with a list containing a function to
## get/set the value of the vector and mean
##--------------------------------------------------------------------------
makeVector <- function(x = numeric()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmean <- function(mean) m <<- mean
  getmean <- function() m
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)
}

##--------------------------------------------------------------------------
## Calculates the mean of the special "vector" created with the makeVector 
## function. It checks to see if the mean has already been calculated. 
## If so, it gets the mean from the cache and skips the computation. 
## Otherwise, it calculates the mean of the data and sets the value of the 
## mean in the cache via the setmean function.
##--------------------------------------------------------------------------
cachemean <- function(x, ...) {
  m <- x$getmean()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- mean(data, ...)
  x$setmean(m)
  m
}