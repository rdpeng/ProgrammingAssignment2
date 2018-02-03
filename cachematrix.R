## Caching The Inverse of a Matrix
## makeCacheMatrix will create four functions to do the following 
## set value of matix
##get value of matrix
## set inverse of a matrix
## get inverse of a matrix



makeCacheMatrix <- function(x = matrix()) {
Inv<-NULL ##NULL value is a position holder for Inv for future reference
set <- function(y) {
  x <<- y  ##y is substituted in place of x in parent env
  Inv <<- NULL ## clears all previous values of Inv
}
get <- function() x  ##lexical scoping picks up x from parent envo
setinverse <- function(inverse) Inv <<- inverse
getinverse <- function() Inv ## Inv is picked up from parent envo after it acquies new value
list(set = set,get = get,setinverse = setinverse,getinverse = getinverse)
}

## CacheSolve calculates inverse,it retrieves the inverse from an object of type makeCacheMAtrix()

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  Inv <- x$getinverse()## retrieve inverse from object passed as argument
  if (!is.null(Inv)) { ##check if Inv is NULL
    message("getting cached data") ## if not NULL we have a valid Inverse and can return it to parent env
    return(Inv)
  }
  data <- x$get() 
  Inv <- solve(data, ...)
  x$setinverse(Inv)
  Inv
  ## CacheSolve() gets matrix from input object it calculates its inverse, 
  ##uses setinverse() on input object to set inverese 
  ## Then returns inverse Inv to parent env
}

