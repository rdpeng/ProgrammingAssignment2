##Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix rather
##than compute it repeatedly. 
##There are also alternatives to matrix inversion that we will not discuss here. 

makeCacheMatrix <- function(x = numeric ()){  
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<-NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set, 
       get = get, 
       setInverse = setInverse,
       getInverse = getInverse)
}

makeVector <- function(x = numeric ()){
  m<- NULL
  set <- function(y){
    x <<- y
    m <<-NULL
  }
  get <- function() x
  setmean <- function(mean) m <<- mean
  getmean <- function() m
  list(set = set, get = get,
     setmean = setmean,
     getmean = getmean)
}

cachemean <- function(x, ...) {
  m <- x$getmean()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- mean(data, ...)
  x$setmean(m)
  m
}
makeCacheMatrix()

makeCacheMatrix <- function(x = numeric()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmean <- function(mean) m <<- mean
  getmean <- function() m
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)
}
## makeCacheMatrix above. If the inverse has already been calculated,
## then it should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of x
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}
## Test My Funtions
    
  my_matrix <- makeCacheMatrix(matrix(1:4,2,2))
my_matrix$get()


my_matrix$set(matrix(c(2, 2, 1, 4), 2, 2))
my_matrix$get()
