makeCacheMatrix <- function(x = matrix()){
  val <- NULL
  set <- function(y){
    x <<- y
    val <<- NULL
  }
  get <- function() {x}
  setInverse <- function(inverse) {val <<- inverse}
  getInverse <- function() {val} 
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

cacheSolve <- function(x, ...){
  val <- x$getInverse()
  if(!is.null(val)){
    message("getting cached data")
    return(val)
  }
  mat <- x$get()
  val <- solve(mat, ...)
  x$setInverse(val)
  val
}
