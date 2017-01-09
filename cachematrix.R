## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
makeCacheMatrix <- function(x = matrix()) {              
  # stores the cached value             
  # initialize to null             
  m <- NULL             
  # matrix created in the working environment              
  set <- function(y) {                       
    x <<- y                      
    m <<- NULL              
  }              
  # get the matrix value               
  get <- function() x              
  # matrix inverted and stored in cache               
  setinverse <- function(solve) m <<- solve            
  # get the inverted matrix from cache               
  getinverse <- function()m              
  # return the created function to the working environment               
  list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}

## Write a short comment describing this function
cacheSolve <- function(x, ...) { 
  ## Return a matrix that is the inverse of 'x'
  ## Get the inverse of the matrix stored in cache
  m <- x$getinverse()
  # return the inverted matrix from cache if it exists else
  # create the matrix in working environment 
  if(!is.null(m)){  
    message ("getting cached data") 
    return(m) 
  } 
  #cache matrix since it does not exist 
  data <- x$get()
  m <- solve(data,...)
  x$setinverse(m) 
  m  
}  
