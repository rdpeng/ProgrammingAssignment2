## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix())  {
  inv <- NULL #initialize an empty "inverse" matrix object
  
  set <- function(y) { # function allows to set a matrix 
    x <<- y  #assign matrix y to matrix object x
    inv <<- NULL #set the inverse matrix object to NULL, but keep it initialized
  }
  get <- function() x #return the matrix
  setinv <- function(solve) inv <<- solve #call the solve function within the "cacheSolve"-function
  getinv <- function() inv  # return the inverse matrix object
  list(set = set, get = get, # provide function commands: get, set, getinv, setinv in form of a list
       setinv= setinv,
       getinv = getinv)
}

## cacheSolve:  This function computes the inverse of the special "matrix" returned by 
##              makeCacheMatrix above. If the inverse has already been calculated 
##              (and the matrix has not changed), 
##              then cacheSolve is retrieving the inverse from the cache.


cacheSolve <- function(x, ...) {
  
    inv <- x$getinv() # get the inverse matrix from the makeCacheMatrix function, if available
    if(!is.null(inv)) { #check is the inverse matrix was already calculated, if so get cached data!
      message("getting cached data") #print message that the function received inverse matrix from cache
      return(inv) #return inverse matrix from cache, stop function at this point
    }
    data <- x$get() #get matrix
    inv <- solve(data, ...) # calculate inverse matrix object
    x$setinv(inv) #write inverse matrix object via command setinv() in x
    inv #forward result as function output
  
}
