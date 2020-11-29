## makeCacheMatrix and cacheSolve functions allow to compute the inverse
## of the given matrix, to store it to cache and to retrieve it from cache 
## if the matrix has not changed

## makeCacheMatrix function returns the list of functions set, get, setInverse
## and getInverse that allow to set and get both the given matrix (x) and the 
## inversed matrix (inv). Basically this is the "cache", in which the values
## of x and inv are stored

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL 
  }
  get <- function() {x}
  setInverse <- function(inverse) {inv <<- inverse}
  getInverse <- function() {inv}
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## cacheSolve function takes x as an argument, where x is a list created
## by the function makeCacheMatrix. cacheSolve function first checks whether
## there is some value stored as a "cache" in makeCacheMatrix: if there
## is some value, it returns it, and if there is no value, it computes
## the inverse of the matrix and adds this value into "cache"

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if(!is.null(inv)){
    message("getting inversed matrix from cache")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInverse(inv)
  inv
}
