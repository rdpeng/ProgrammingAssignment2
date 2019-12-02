makeCacheMatrix <- function(A = matrix()) {
  Inver <- NULL
  set <- function(C){
    A <- C
    Inver<- NULL
  }
  get <- function()x
  setInverse <- function(inverse) Inver <<- inverse
  getInverse <- function() Inver 
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}
cacheSolve <- function(A, ...) {
 
  Inver <- A$getInverse()
  if(!is.null(Inver)){
    message("Attraper la Data")
    return(Inver)
  }
  B <- A$get()
  Inver <- solve(B,...)
  A$setInverse(Inver)
  Inver
}
