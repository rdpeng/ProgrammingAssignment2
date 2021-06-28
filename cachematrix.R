makeCacheMatrix <- function(p = matirx()){
  inv <- NULL
  set <- function(q){
    p <<- q
    inv <<- NULL
  }
  get <- function() {x}
  setInverse <- function(inverse) {inv<<- inverse}
  getInverse <<- function() {inv}
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

cacheSolve <- function(p,...){
  inv <- p$getInverse()
  if(!is.NULL(inv)){
    messgae("getting cached data")
    return(inv)
  }
  mat <- p$get()
  inv <- solve(mat, ...)
  p$setInverse(inv)
  inv
}