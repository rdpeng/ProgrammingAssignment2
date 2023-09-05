## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

matrix_inv <- function(x = matrix()){
  inv <- NULL
  mat <- x
  
  #function to set matrix data
  set_matrix <- function(y){
    x <<- y
    inv <- NULL
  }
  
  #Get the matrix data
  get_matrix <- function()mat
  #set inverse of the matrix
  set_inv <- function(inverse){
    inv <- inverse
  }
  
  # Function to get the cached inverse, if available; otherwise, calculate it
  get_inv <- function() {
    if (!is.null(inv)) {
      # If the inverse is cached, return it
      return(inv)
    } else {
      # If not cached, calculate the inverse and cache it
      inv <<- solve(mat)
      return(inv)
    }
  }
 
  
  # Return a list of functions for interacting with the matrix and its inverse
  list(set_matrix = set_matrix, get_matrix = get_matrix, set_inv = set_inv, get_inv = get_inv)
  
}



#Cached matrix inverse
cached_matrix <- function(cache){
  inv <- cache$get_inv()
  
  if(!is.null(inv)){
    message("getting cached inverse")
    return(inv)
  }else{
    message("Calculating the inverse result")
    inv <- solve(cache$get_matrix())
    cache$set_inv(inv)
    retrun(inv)
  }
