## create a function storing a list of functions to set/get a matrix and get/set the 
## inverse of the matrix and a second function to calculate the inverse 
## (if not already calculated)


## function storing a list of functions

makeCacheMatrix <- function(x = matrix()) { 

  inv <- NULL ##inv is the variable storing the inverse
  set <- function(y) { ## set the matrix to be inversed
    x <<- y
    inv <<- NULL
  }
  get <- function() { ## get the matrix to be inversed
    x
  }
  setinv <- function(i) { ## set the inverse
    inv <<- i
  }
  getinv <- function() { ## get the inverse
    inv
  }  
  list(set = set, get = get, setinv = setinv, getinv = getinv) ##create a list of the functions stored
}


## function that returns the inverse cached or calculates the inverse if not already cached

cacheSolve <- function(x, ...) {
  o<-x$getinv()     ## set o to cached inverse
  if(!is.null(o)) { ## check if cached
    message("getting cached data")
    return(o) ## ends function
  }
  data <- x$get() ## get matrix stored
  inv <- solve(data) ## take the inverse
  x$setinv(inv) ## store as inverse
  inv ## return the inverse
}
