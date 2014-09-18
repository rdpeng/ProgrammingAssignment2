## Put comments here that give an overall description of what your
## functions do
##creates a matrix  that creates the inverse of a matrix and stores it in a cache.
##

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) { #input is a matrix
  
  m <- NULL #initiate
  
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
##first checks if the inverse has been calculated and then either retrieves the result from cache or calculates the inverse.
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
