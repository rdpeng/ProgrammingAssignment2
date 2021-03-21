#these functions set/get the value of the matrix and the value of its inverse

#this will create a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

#this calculates the inverse of the special "matrix" created with the above function
#it first checks to see if the inverse has already been calculated
#if the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache
#if not, it calculates the inverse of the matrix and sets the value of the inverse in the cache via the setinverse function
cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if (!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  mat <- x$get()
  i <- solve(mat, ...)
  x$setinverse(i)
  i
}
