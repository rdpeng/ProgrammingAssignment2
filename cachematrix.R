makeCacheMatrix <- function(x = matrix()) {
##'x' is a square invertible matrix
## it should return a list containing functions to set and get the matrix...
##...and then set and get the inverse
## this list is used as the imput to cacheSolve()
  inv = NULL
  set = function(y){
    x <<- y
    inv <<- NULL
##'<<-' assings a value to an object in an enviroment different from the current...
##...enviroment   
  }
  get = function() x
  setinv = function(inverse) inv <<- inverse
  getinv = function() inv
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}

cacheSolve <- function(x, ...) {
##'x' is the output of makeCacheMatrix()
## it should return the inverse of the original matrix imput to makeCacheMatrix()
  
  inv = x$getinv()
## if the inverse has already been calculated
  if(!is.null(inv)){
## get it from the cache and skips the computation
  message("getting cached data")
  return(inv)  
  }
## calculate the inverse, otherwise
  mat.data = x$get()
  inv = solve(mat.data, ...)
## sets the value of the inverse in the cache via the setinv function
  x$setinv(inv)
  
  return(inv)
}
