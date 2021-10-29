makeCacheMatrix <- function(x = matrix()){
  mig <- NULL
  set <- function(y){
    x <<- y
    mig <<- NULL
  }
  get <- function() {x}
  setInverse <- function(inverse) {mig <<- inverse}
  getInverse <- function() {mig} 
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

cacheSolve <- function(x, ...){
  mig <- x$getInverse()
  if(!is.null(mig)){
    message("getting cached data")
    return(mig)
  }
  mat <- x$get()
  mig <- solve(mat, ...)
  x$setInverse(mig)
  mig
}
