## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
### Part 1:
### makeCacheMatrix: 
#This function creates a special "matrix" object that can cache its inverse. 

makeCacheMatrix <- function(x = matrix()) {
  # sets x equal to an empty matrix 

  # Set the inverse equal to NULL 
  I <- NULL 

  set <- function(y){ 
    # set function assigns the argument to x
    x <<- y 
    
    # Once the set function is called, Inverse is re-set to NULL (this is important if you redefine the matrix, x) 
    I <<- NULL 
  } 
  
  # get function returns the matrix 
  get <- function() x 
  
  # setInverse overrides the previous value of I and assigns the argument to Inverse (which is supposed to be the inverse of matrix x) 
  setInverse <- function(solve) I <<- solve 
  
  # getInverse returns the Inverse 
  getInverse <- function() I 
  
  # creates a list of the functions 
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse) 

}


## Write a short comment describing this function
### Part 2:
# cacheSolve: 
# This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
# If the inverse has already been calculated (and the matrix has not changed), 
# then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  # Retrives the most recent value for the inverse 
  I <- x$getInverse() 
  
  if(!is.null(I)){ 
    message("getting cached data") 
    
    # If the value of Inverse is NOT null (was previously calculated), cacheSolve returns that value
    return(I) 
  } 
  
  # If the value of Inverse is NULL, then you retrive matrix x 
  # and calculate the inverse with the solve() function 
  
  message("newly calculating data") 
  data <- x$get() 
  I <- solve(data, ...) 
  
  # Sets Inverse to the newly calculated value    
  x$setInverse(I) 
  
  #Returns the new Inverse value 
  return(I) 
}
