##### I wrote this matrix function to give it ability to cache the inverse
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}
#####This function computes the inverse of the special "matrix" returned by `makeCacheMatrix` above. 
#####If the inverse has already been calculated (and the matrix has not changed), then the `cachesolve` should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if (!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
my_data <- matrix(c(1,2,3,4),2,2)
my_data2<- makeCacheMatrix(my_data)
cacheSolve(my_data2)
cacheSolve(my_data2)
#####stop
