## the makeCacheMatrix function takes a matrix and can set and get its reverse 
## within a list. The cacheSolve can take this special matrix and read the 
## inverse within (if it is there) or calculate the inverse (if not there).

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()){
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(solve) m <<-solve
  getinv <-function() m
  list(set = set, get =get, setinv = setinv, getinv =getinv)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and 
## the matrix has not changed), then the cachesolve should retrieve the 
## inverse from the cache.
cacheSolve <- function(x, ...) {
          m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setinv(m)
  m
}

## for test
test <- rbind(c(1,3),c(5,7))
testCache <- makeCacheMatrix(test)
result <- cacheinv(testCache)
test %*% result
