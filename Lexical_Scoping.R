

# Lexical Scoping - Programming Assignment 2


     ##'makeCacheMatrix' funtion will create special "matrix" object
        #that can cache its inverse.
 makeCacheMatrix <- function(mat = matrix()) 
  {
  
  inv_matrix <- NULL
  
  set <- function(y) 
    {
    mat <<- y
    inv_matrix <<- NULL
    }
  
  get <- function() 
    {  mat  }
  
  setinv <- function(inv) 
    { inv_matrix <<- inv }
  
  getinv <- function() 
    { inv_matrix }
  
  list(set = set, get = get, setinv = setinv, getinv = getinv)
  
}

 ## This function computes the inverse of the special "matrix"  
     #returned by makeCacheMatrix above
cacheSolve <- function(mat, ...) 
{
  inv_matrix <- mat$getinv()
  
  if(!is.null(inv_matrix)) 
    {
      message("getting cached data")
      return(inv_matrix)
    }
  
  data <- mat$get()
  inv_matrix <- solve(data)
  mat$setinv(inv_matrix)
  inv_matrix 
  
}