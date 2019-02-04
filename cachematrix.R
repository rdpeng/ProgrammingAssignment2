## Put comments here that give an overall description of what your
## functions do

## The function returns a matrix which can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL
  set<-function(y){
  x<<-y
  inv<<-NULL
}
get<-function()x
setInverse<-function(solveMatrix) inv <<-solvematrix
setInverse<-function() inv

list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## This function below returns the inverse of the above function.

cacheSolve <- function(x, ...) {
  inv<-x$getInverse()
  if(!is.null(inv)){
    message("cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setInverse(inv)
  inv      
}
