makeCacheMatrix <- function(x = matrix()){
  
  #Declare parameter
  mat.inverse <- NULL
  
  #Set the matrix
  set <- function(y){
    mat <<- y
    mat.inverse <- NULL
  }
  
  #Get the matrix
  get <- function(){
    mat
  }
  
  #Set the inverse matrix
  setinverse <- function(inverse){
    mat.inverse <<- inverse
  }
  
  #Get the inverse matrix
  getinverse <- function(){
    mat.inverse
  }
  
  
  
}