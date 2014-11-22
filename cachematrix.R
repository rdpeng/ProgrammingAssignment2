######################################################
# makeCacheMatrix
# https://class.coursera.org/rprog-009/human_grading/view/courses/972583/assessments/3/submissions
# Purpose: Returns a list of functions stored in a list object
# 
# Information: <<- operator which can be used to assign a value to an 
# object in an environment that is different from the current environment.
######################################################
makeCacheMatrix <- function(x) {
  matrix.inverse <- NULL  #matrix.inverse set to NULL
  
  set <- function(y) {
    x <<- y
    print("Setting m to NULL")
    matrix.inverse <<- NULL #Set to null so we can recalculate a new matrix inverse
  }
  
  #get is one line function that returns the vector that was passed in and cached for the object
  get <- function() {x}
  
  #setinverse is a one line function to return to set the inverse of a square matrix
  setinverse <- function(inverse) 
  {
    
    print("Calling setinverse. Caching inverse.....")
    matrix.inverse <<- inverse #Cache the latest value for the inverse
    
  }
  
  #getinverse is a one line function to return to get the cached inverse of matrix
  getinverse <- function() {matrix.inverse}
  
  #Store all functions in a list and return the list
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}



cacheSolve <- function(x, ...) {
  
  minv <- x$getinverse()
  
  #If there is a value in cache retrieve that value else solve for a new matrix value
  if(!is.null(minv)) 
  {
    message("getting cached inverse of matrix")
    return(minv)
  }
  
  #Getter method to retrieve the arguments that were set
  data <- x$get()
  
  #Run solve function on the matrix
  minv <- solve(data)
 
  #Call setinverse on makeCacheMatrix object to cache the inverse
  x$setinverse(minv) # cache the value so as not to re-compute unless value changes
  
  #Return the inverse
  minv
}

getMatrix <- function(x) {
  
  return(x$getinverse())
  
}

