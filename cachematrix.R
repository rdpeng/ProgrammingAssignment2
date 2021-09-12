makeCacheMatrix <- function(x = matrix()) {## Assigning Function
  i <- NULL                             ## initial value i which stores matrix inverse 
  set <- function(y) {                    ## set function
    x <<- y                             ## matrix value check
    i <<- NULL                        ## if there is new matrix data, set i to NULL
  }
  get <- function() x                     ## define the get function
  
  setinverse <- function(inverse) i <<- inverse  ## assigns value of inverse, i
  getinverse <- function() i                    ## get the value of inverse
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)  
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()     ## Assigning inverse,i
  if(!is.null(i)) {               ## Checking condition, if true
    message("getting cached data")
    return(i)
  }
  data <- x$get()                  ## Checking condition, if false
  i <- solve(data, ...)            ## compute inverse
  x$setinverse(i)
  i
}
