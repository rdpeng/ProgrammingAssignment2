
# creates special vector which is a list containing function
makeCacheMatrix <- function(x = matrix()) {
  # set the value of the vector
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  # get the value of the vector
  get <- function() x
  # set value of inverse
  setInverse<- function(inverse) inv <<- inverse
  # get value of inverse
  getInverse <- function() inv
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}
  

# calculates the inverse of the special vector created with above function
cacheSolve <- function(x, ...) {
  # checks if inverse already calculated
  inv <- x$getInverse()
  if(!is.null(inv)) {
    # if already calculated, then shows that one along with message 
    message("getting cached data")
    return(inv)
  }
  # if not, then calculate inverse
  data <- x$get()
  inv <- solve(data, ...)
  x$setInverse(inv)
  inv
}
