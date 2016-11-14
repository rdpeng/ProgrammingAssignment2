#The calculation of the inverse of an array can generate a high computational cost,
#so in more sophisticated programs this can be a problem, 
#therefore this script caches the inverse of an array

#makeCacheMatrix contains the following functions: the value of a matrix is loaded, 
#the inverse of the matrix is calculated and the initial 
#matrix and the inverse matrix are obtained
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


#cacheSolve function calculates the inverse of an array and allows to
#obtain the result, this function assumes that the matrix has 
#inverse would otherwise generate an error
cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data.")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}
