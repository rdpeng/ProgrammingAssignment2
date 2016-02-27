## makeCacheMatrix makes a matrix that can be cached. cacheSolve returns the inverse of this matrix object

## makes a matrix object that can cache its matrix

makeCacheMatrix <- function(x = matrix()) {
  inv = NULL
  set = function(y) {
    x <<- y
    inv <<- NULL
  }
  get = function() x
  setinv = function(inverse) inv <<- inverse 
  getinv = function() inv
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}



## returns a matrix that is the inverse of the variable x

cacheSolve <- function(x, ...) {      
  inv = x$getinv()
  
  
  if (!is.null(inv)){
    message("getting cached data")
    return(inv)}
  {mat.data = x$get()
  inv = solve(mat.data, ...)
  
  x$setinv(inv)
  
  return(inv)}

 
 
  

