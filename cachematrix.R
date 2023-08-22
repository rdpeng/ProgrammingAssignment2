
# loading data and used MASS dataset 
library(MASS)

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL # setting inverse as NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function(){
    inver <- ginv(x)
    inver%*%x # to get an inverse of the matrix
  }
  
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

# getting the cache data

cacheSolve <- function(x, ...)  ## to get the cache data
{
  inv<-x$getinv()
  if(!is.null(inv)){ 
    message("geting cache data!")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data,...)
  x$setinv(inv)
  inv ## output is a matrix of inverse of x
}
dk <- makeCacheMatrix(matrix(1:4, 2, 4))
dk$get()
dk$getinv()
cacheSolve(dk)

