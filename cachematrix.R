# The makeCacheMatrix function creates a special 1*4 "matrix" (actually a list) that contains 4 functions to:
#1- set the value of a matrix
#2- get the value of a matrix
#3- set the value of the inverse matrix
#4- get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) # it can be called with or without arguments
{
  inv <- NULL
  
  set <- function(y) 
  {
    x <<- y                           # since we are just setting a new matrix, the inverse hasn't been calculated yet
    inv <<- NULL                      # so we initialize it to NULL. We use <<- to make the variables visible in other scopes
  
  }
  
  get <- function()
  {
    x  
  } 
  
  setinv <- function(inverse)
  {
    inv <<- inverse
  }
  
  getinv <- function()
  {
    inv
  }
  
  list(SET = set, GET = get, SETINV = setinv, GETINV = getinv) # return the list of functions with capitalized labels
  
}

#The cacheSolve function calculates the inverse of the matrix that is set by the special "matrix" of the previous function. However, it first checks to see if the inverse has already been calculated. If so, it gets the inverse from the cache and skips the calculations. Otherwise, it calculates the inverse of the matrix and sets the value of the inverse in the cache via 'setinv'
cacheSolve <- function(x, ...) 
{
  inv<- (x$GETINV)()
  
  if(is.null(inv))                            #if the mean was not cached (NULL)
  {
    mat <- x$GET()                            #retrieve the data matrix  
    inv <- solve(mat)                         #solve it and set the inverse value in the cache
    x$SETINV(inv)    
  }
  else                                        #otherwise, show the cache message
  {
    message("getting cached data")
  }
  
  return(inv)
}
