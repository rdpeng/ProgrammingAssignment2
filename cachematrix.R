## Put comments here that give an overall description of what your
## functions do
##
##These functions create a special object that stores a matrix and caches 
##its inverse.
##

## Write a short comment describing this function
##This function creates a special "matrix", which is really a list containing a 
##function to set the value of the matrix,get the value of the matrix,
##set the value of the inverse matrix, and get the value of that inverse.
makeCacheMatrix <- function(x = matrix()) { #input is a matrix
  
  m <- NULL #initiate empty var
  
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<-solve
  getinverse <- function() m
  list(set = set, 
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  


}


## Write a short comment describing this function
##This function calculates the inverse of the special "matrix" created with 
##the above function. However, it first checks to see if the inverse 
##has already been calculated. If so, it gets the inverse from the cache 
##and skips the computation. Otherwise, it calculates the inverse of the 
##matrix and sets the value of the inverse in the cache via the setinverse 
##function.

cacheSolve <- function(x,...) {
        ## Return a matrix that is the inverse of 'x'
        m<-x$getinverse() ##check in cache
  
        if(!is.null(m)){ ## if something is in cache
                message("getting cached data") ## tell us
                return(m)               #and give the result
        }
  
        matrix<-x$get()    
        m<-solve(matrix, ...) #calculate inverse of the matrix
        x$setinverse(m) #put result in cache
        m
}
