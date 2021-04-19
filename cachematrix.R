makeCacheMatrix <- function(x = matrix()){
  fai <- NULL
  set <- function(y){
    x <<- y
    fai <<- NULL
  }
  get <- function() {x}
  setInverse <- function(inverse) {fai <<- inverse}
  getInverse <- function() {fai} 
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

cacheSolve <- function(x, ...){
  fai <- x$getInverse()
  if(!is.null(val)){
    message("getting cached data")
    return(fai)
  }
  mat <- x$get()
  fai <- solve(mat, ...)
  x$setInverse(fai)
  fai
}
