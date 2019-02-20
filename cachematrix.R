##Matrix inversion is usually a costly computation and 
##there may be some benefit to caching the inverse of a matrix 
##rather than computing it repeatedly


#The below function caching the inverse of it's matrix

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <-function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

##This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
##If the inverse has already been calculated (and the matrix has not changed), 
##then cacheSolve should retrieve the inverse from the cache.
cachesolve <- function(x,...){
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  mat <- x$get()
  i <-solve(mat,...)
  x$setinverse(i)
  i
}

f <- makeCacheMatrix(matrix(c(5,10,15,20), 2, 2))

f$get()

f$getinverse()

cachesolve(f)



