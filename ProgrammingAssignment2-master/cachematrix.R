## With this function we cache the matrix we are using
## First the actual matrix is saved (so invmat is null),   
##and then we inverse it in the second part of the function

makeCacheMatrix <- function(x = matrix()) {
  invmat <- NULL
  set <- function(y){
    x <<- y
    invmat <<- NULL
  }
  get <- function()x {
  setInverse <- function(inverse) invmat <<- inverse
  getInverse <- function() invmat 
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
  }
  }
## With this function, if the matrix is already cached 
##then it is returned automatically, if not it is calculated here

cacheSolve <- function(x, ...) {
  invmat <- x$getInverse()
  if(!is.null(invmat)){
    message("getting cached data")
    return(invmat)
  }
  mat <- x$get()
  invmat <- solve(mat,...)
  x$setInverse(invmat)
  invmat
}


