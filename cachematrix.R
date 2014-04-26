## Ignacio Gonzalez Castañeda
## 2014
## For the R Programming Course, Peer Assessments
## by Roger D. Peng, Brian Caffo, PhD, Jeff Leek

##This function save the result of a inverse of a square matrix in a object 
##Aditional Note: this style of programming is OO

makeCacheMatrix <- function(x = matrix()) {
  invMatrix <- NULL ##this is the property that store the result and is null when is recently created
  set <- function(y) ##this function set the original matrix 
  {
    x<<-y
    invMatrix <<- NULL ##because the original matrix change the invMatrix is set to null
  }
  get<-function() x ##function that return the original matrix
  setInvMatrix <-function(invMatrixResult) ##function that set the value of inverse matrix
  {
    invMatrix <<- invMatrixResult
  }
  getInvMatrix <- function() ##function that return the value of invMatrix property
  {
    invMatrix
  }
  ##Creates a list with the name of the accesible functions
  list(set=set,get=get,
       setInvMatrix= setInvMatrix,
       getInvMatrix = getInvMatrix)

}


## this funtion compute the inverse of a matrix

cacheSolve <- function(x, ...) {        
  invMatrix = x$getInvMatrix() ## get the property of x that store the inverse matrix
  if(!is.null(invMatrix)) ##if the value returned is not null
  {
      message("The inverse matrix cached value is returned") ##Message for demostrative purposes
      return(invMatrix) ##return the invMatrix of x that is stored
  }  
  ##this code is executed if the condition above is false
  ##not is necessary declare a else, because the code inside the if returns a value for the funtion
  matrixData <- x$get() ##get the property of x that store the original matrix
  invMatrix <- solve(matrixData) ##compute the inverse of square matrix
  x$setInvMatrix(invMatrix) ##set the value of inverse matrix in the object x, for cache the result
  invMatrix ##return the inverse matrix 
}
