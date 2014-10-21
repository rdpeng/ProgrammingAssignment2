

# Lexical Scoping - Programming Assignment 2


     ##'makeCacheMatrix' funtion will create special "matrix" object
        #that can cache its inverse.
 makeCacheMatrix <- function(mat = matrix()) 
  {
    # this is the cache matrix
  inv_matrix <- NULL
  
    # 'Set' function will set new values to the matrix 'mat' and resets the 'inv_matrix' to NULL
  set <- function(y) 
    {
    mat <<- y
    inv_matrix <<- NULL
    }
  
    #'get' function will receive the matrix 'mat' from the formal argument of 
      #'makeCacheMatrix' function
  get <- function() 
    {  mat  }
  
    #'setinv' function will set the inverse of a matrix to 'inv_matrix' which acts as the 
      # cached data  
  setinv <- function(inv) 
    { inv_matrix <<- inv }
  
    #'getinv' function will return the cached data 
  getinv <- function() 
    { inv_matrix }
  
  list(set = set, get = get, setinv = setinv, getinv = getinv)
  
}
 

 ## This function returns  the inverse of the special "matrix" 
    
cacheSolve <- function(mat, ...) 
{
  # checking for the cached data from 'getinv' function      
  inv_matrix <- mat$getinv()
  
  #checks and returns the cached data if it's not NULL value
  if(!is.null(inv_matrix)) 
    {
      message("Getting cached data ....")
      return(inv_matrix)
    }
  
  # getting the matrix to compute the inverse of it
  data <- mat$get()
  
  # finding the inverse of the matrix 
  inv_matrix <- solve(data)
  
  # as there is no cache data, the below function will call 'setinv' function to 
     # set the inverse value 
  mat$setinv(inv_matrix)
  
  # returns the inverse after setting the value to the cached matrix
  inv_matrix 
  
}