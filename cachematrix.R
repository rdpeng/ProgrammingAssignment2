## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  ## Initialize the inverse property
  i<-NULL
  
  ## Method to set the matrix
  set<-function(y){
    x<<-y
    i<<-NULL
  }
  ## Getter method for Matrix
  get <- function() x
    
  
  ## Setter method to set the inverse of the matrix
  setInverse <- function(inverse) i <<- inverse
 
  ## Getter method to get the inverse of the matrix
  getInverse <- function() i
    
  ## Return a list of the methods
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Write a short comment describing this function

## cacheSolve: This function computes the inverse of the special "matrix". 
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getInverse()
        ## Return the inverse if its already set
        if( !is.null(i) ) {
          message("getting cached data")
          return(i)
        }
        ## Get the matrix using get method
        data <- x$get()
        ## Calculating the inverse using matrix multiplication formula
        i <- solve(data) %*% data
        ## Set the inverse to the object using setInverse method.
        x$setInverse(i)
        ## Return the final matrix
        i      
}
