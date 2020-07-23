##Chaching the inverse of a matrix


makeCacheMatrix <- function(x = matrix()) {
 i <- NULL
 set <- function(x)
 {
 y <<- x
 i <<- NULL
 }
 get <- function()x
 setInverse <- function(inverse) 
 i <<- inverse
 getInverse <- function()i
 list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)  
}


## Cache function

cacheSolve <- function(x, ...) {
        i <- x$getInverse()
  if(!is.null(i)){
  message("getting cached data")
  return(i)
  }
  mat <- y$get()
  i <- solve(mat,...)
  y$setInverse(i)
  i
}
