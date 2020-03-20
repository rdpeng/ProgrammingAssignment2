## Put comments here that give an overall description of what your
## functions do

## the funtion MakeCachedMatrix takes as input a matrix, determines whether the matrix inversible and returns and inverse if true.

makeCacheMatrix <- function(x = matrix()) {
  if(ncol(matx) == nrow(x) && det(x) != 0){
    set <- function(y){
      x <<- y
      m <<- NULL
    }
    get <- function() x
    setinverse <- function() m <<- inv(x)
    getinverse <- function() m
    list( set = set, get = get, setinverse = setinverse, getinverse = getinverse)
  }else{
    return (message("matrix is not inversible"))
  }
}


## This function in wanting to calculate the inverse of a matrix, looks in the cache if the inverse has been calculated. I calculates if not, a returns a value from the cache if available.

cacheSolve <- function(x, ...) {
  m <- x$getinverse
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }else{
    data <- matx$get
    m <- inv(data, ...)
    x$setinverse(m)
    m
  }
}
