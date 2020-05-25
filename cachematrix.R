#function creat  special matrix 

# fix a mistake : "j" instead of "a"

makeCacheMatrix <- function(x = matrix()) {
  j <- NULL 
  set <- function(y){
    x <<- y
    j <<- NULL
  }
  get <- function()x
  setInverse <- function(inverse) j <<- inverse
  getInverse <- function() j 
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}

#function making an inverse matrix

cacheSolve <- function(x, ...) {

  j <- x$getInverse()
  if(!is.null(j)){
    message("getting cached data")
    return(j)
  }
  mat <- x$get()
  j <- solve(mat,...)
  x$setInverse(j)
  j
}

#Application
#insert a square matrix 3x3 (example)
x <- c(2,2,1,3,2,4,2,5,6)
dim(x) <- c(3,3)
#compute a cache matrix
cache <- makeCacheMatrix(x)
#compute an inverse matrix
cacheSolve(cache)


