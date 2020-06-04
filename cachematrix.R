## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL     ##It represent the null object
  set <- function(y) {    ##it set the value of matrix using another function
    x <<- y
    inv <<- NULL    ##modifible variable
  }
  get <- function() {x}
  setInverse<- function(inverse) {inv <<-inverse} ##set value of setInverse
  getInverse <- function() {inv}    ##get value of inv
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse() ##getting the cache data
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...) ## to compute inverse of the matrix we use solve a standard function of R
  x$setInverse(inv)
  inv
}
