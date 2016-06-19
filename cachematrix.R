## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
makeCacheMatrix <- function(x = matrix()) 

{
  
    ma <- NULL   ## Initializing MA                                   
  
    ## set function is to keep global_x and global_m as passed matrix and Null, respectively.
    
    set <- function(y) 
    {
  
      # y is the initial matrix from user. so it is stored in global_x.
    
      global_x <<- y 
    
      # initialize global_ma 
    
      global_ma <<- NULL                                
    }
  
  # Create one line function(). the matrix stored by set() is returned.
  get <- function() return(global_x)
  # Create one line function(). the matrix will be stored as global value.
  set_global_ma <- function(m) global_ma <<- ma    
  # Create one line function(). the matrix stored by set_global_ma() is returned.
  get_global_ma <- function() return(global_ma)             
  
  list(set = set, get = get,
       set_global_ma = set_global_ma,
       get_global_ma = get_global_ma)

}




## Write a short comment describing this function

cacheSolve <- function(x, ...) 
{
  
  
  # try to get the value from the global environment.
  ma<- x$get_global_ma()
  # Check the result.
  if(!is.null(ma)) { 
    
    # BY checking if m is NULL, we shall know whether this matrix was  computed already or not.
    # if so, return computed value, then print the message.
    
    message("getting cached data")
    
    return(ma)
  }
  # if m is NULL, the inverse of matrix is computed through solve() function.
  # Then, this result will be stored in global value for reuse.
  data <- x$get()               
  inverseMatrix <- solve(data)   
  x$set_global_m(inverseMatrix)             
  return(inverseMatrix)          
        #Finally Return the matrix that is the inverse of 'x'
}

