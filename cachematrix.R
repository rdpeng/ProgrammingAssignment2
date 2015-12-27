## Two below functions store the inverse matrix results, and solve inverse matrix equations, respectively
## makeCacheMatrix has four functions to store the inverse values and set the inverse values
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y){ # change the value to new input
    x <<- y # changes the value of  x to the new input y using cbind
    i <<- NULL # discard the value of old_inverse_value since new value y assumes to x
  }
  get <- function()x # store the value from the above set
  setinverse <- function(inverse) i <<-inverse # store the value of inverse to i for use in set(y)
  getinverse <- function() i # get the  value from setinverse function
  list(set = set, get= get,
       setinverse = setinverse,
       getinverse = getinverse)
}

#cacheSolve returns the inverse matrix if already present or compute it if there is not inverse
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse
  if (!is.null(i)){ # checking if inverse is already calculated
    message("getting cached inverse data")
    return(i)
  }
  data <- x$get()
  i <- solve(data,...)
  x$setinverse(i)
  i  
}

