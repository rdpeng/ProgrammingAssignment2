##
## makeCacheMatrix ()
#   Creates a cached matrix object
## Compute the Inverse of a Matrix (but only once)
## Check to see if the inverse has already been calculated.  
##   If so - return the cached inverse
##   otherwise - calculate the inverse and set the inverse in the cache
## 
##  setinverse ()  - set the value of the cache matrix
##  getinverse ()  - get the value of inverse matrix
##  

makeCacheMatrix <- function(x = matrix()) {
  
    inv <- NULL  # set inverse to NULL
    
    # define a function to set inverse to NULL
    set <- function(y) { 
      x <<- y             #  set x equal to y
      inv <<- NULL
    }
    
    # define a function to return x
    get <- function() x   
    
    # define a function to set inv to inverse using solve ()
    setinverse <- function(solve) inv <<- solve  
    
    # define a function to return the inverse, inv
    getinverse <- function() inv                 # get cached value
    
    #  create a list of functions defined above
    list(set = set, get = get,
         setinverse = setinverse, 
         getinverse = getinverse) 
  }


##
## cacheSolve ()
## 
## Check to see if the inverse has already been calculated.  
##   If so - return the cached inverse
##   otherwise - calculate the inverse of the "special" matrix
##        and set the inverse in the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  invFun <- x$getinverse()
  if(!is.null(invFun)) {
    print("getting cached data")
    return(invFun)
  }
  data <- x$get()
  invFun<- solve(data, ...)
  x$setinverse(invFun)
  invFun
  
}

