#Functions#
#makeCacheMatrix : This function produces a cache matrix for storing the matrix and cache its inverse
#cacheSolve: This function is used to retrieve the inverse of matrix if exists in cache of the matrix else compute and store one in the cache matrix

#cache matrix creation function for storing inverse in cache memory or list(in this case). Takes in matrix as input and list of functions as output
makeCacheMatrix <- function(matrix_ = matrix()){
  #cache inverse matrix initialized to null
  inv_matrix <- NULL
  
  #getter and setter functions for matrix
  set <- function(y) {
    matrix_ <<- y
    inv_matrix <<- NULL
  }
  get <- function() matrix_
  
  #getter and setter functions for cached inverse matrix
  setinverse <- function(inv) {
    inv_matrix <<- inv
  }
  getinverse <- function() {
    inv_matrix
  }
  
  #return the list of getter and setter functions for the outside env to interact with this new created cache matrix
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

#cache solve function returns the cached inverse if it exists else it computes the inverse and stores it in cache memory
cacheSolve <- function(matrix_, ...) {
  
  #try and retrieve the cached inverse matrix
  inv_matrix <- matrix_$getinverse()
  
  #if the cache inverse matrix exists then return the cached inverse
  if(!is.null(inv_matrix)){
    message("getting cached matrix inverse")
    inv_matrix
  }
  
  #retrieve the matrix and assign it to temp, then compute the inverse and assign it to inverse matrix
  temp_matrix <- matrix_$get()
  inv_matrix <- solve(temp_matrix, ...)
  
  #cache the inverse matrix back into the original cache matrix
  matrix_$setinverse(inv_matrix)
  
  #return the inverse matrix quietly
  invisible(inv_matrix)
}
