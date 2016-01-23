makeCacheMatrix <- function(x=matrix()){
  
  #FUNCTIONALITY This function creates a special "matrix" object that can cache its inverse. 
  #STRUCTURE Using the getter setter functionalities like in Java Programming
  #NOTE: I'm using the same skeleton as the example given in the assignment statements
  
  local_inverse_matrix <- NULL # Initializing the inverse matrix
  
  #Creating getter and setter frunctions for the input x
  set <- function(ip_matrix){
    cache_matrix <<- ip_matrix # Sets the ip into the variable x within the function
    cache_inverse_matrix <<- NULL
  }

  get <- function(){
    cache_matrix # return the value x
  }
    
  #Creating the getter and setter functions for the output variable op_inverse_matrix
  setInverse <- function(local_inverse_matrix){
    cache_inverse_matrix <<- local_inverse_matrix # Sets the value for the inverse matrix
  }
  getInverse <- function(){
    cache_inverse_matrix # returns the output variable op_inverse_matrix
  }
  
  #Creating the list with getter and setter functions 
  list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}

cacheSolve <- function(x, ...){
  #FUNCTIONALITY This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has al
  #STRUCTURE Using the getter ans the setter functionalities like in Java programming
  #NOTE:  I'm using the same skeleton as the example given in the assignment statements
  
  local_inverse_matrix <- x$getInverse() # Calling the getter function for reading the inverse matrix value
  if(!is.null(local_inverse_matrix)){ # Checking whether the inverse_matrix variable is not null
    message("getting cached data") # Printinf to the user that the data obtained is from cache
    return(local_inverse_matrix) # returning the cached inverse matrix
  }
  data_matrix<-x$get() # Getting the input matirx 
  inverse_matrix<-solve(data_matrix) # Receiving the inverse matrix from the Solve function
  x$setInverse(inverse_matrix) # Caching the inverse matrix
  inverse_matrix # Returning the inverse matrix
}
