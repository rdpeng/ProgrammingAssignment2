##  Below pair of functions cache the inverse of a matrix assuming that the matrix supplied is invertible 

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
cache<-NULL
  set<- function(y){
    x<<-y
    cache<<-NULL
  }
  get<- function()x
  setMatrix <- function(inverse) cache <<- inverse
  getInverse <- function() cache
  list(set = set, get = get, setMatrix = setMatrix, getInverse = getInverse) 
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then cachesolve
## retrieves the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        cache <- x$getInverse()
  if (!is.null(cache)) {
    return(cache)
  }
  matrix <- x$get()
  cache <- solve(matrix, ...)
  x$setMatrix(cache)
  return (cache)
}
