## pair of functions that cache the inverse of a matrix.
	
## This function creates a special "matrix" object that can cache its inverse.


makecachematrix <- function(x = matrix()){
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function(){x}
  setinverse <- function(inverse) {inv <- inverse}
  getinverse <- function() {inv}
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse) 
}

## This function computes the inverse of the specialc"matrix" returned by `makeCacheMatrix` above. If the inverse has
## already been calculated (and the matrix has not changed), then
##`cacheSolve` should retrieve the inverse from the cache.


chachesolve <- function (x, ...){
  inv <- x$getinverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setinverse(inv)
  inve
} 
