## Function makeCacheMatrix defines the functions to extract the matrix passed as parameter, 
## compute and store the inverse of the matrix to cache and retrieve the inverse of the matrix from cache
## Function cacheSolve checks if the inverse of the matrix has already available in the cache and if it is 
## available, it is retrieved from cache. If the inverse is not available in cache, the inverse is computed 
## and it is stored in cache



makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setInverse <- function(solve) i <<- solve
  getInverse <- function() i
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)

}




cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getInverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setInverse(i)
  i
}


## Sample Run
##
##mat<-matrix(c(1,3,0,0,7,2,2,9,1),nrow=3,byrow=TRUE)
##h<-makeCacheMatrix(mat)
##cacheSolve(h)
##Expected Output:
##     [,1] [,2] [,3]
##[1,]  -11   -3    6
##[2,]    4    1   -2
##[3,]  -14   -3    7
##cacheSolve(h)
##Expected Output:
##getting cached data
##     [,1] [,2] [,3]
##[1,]  -11   -3    6
##[2,]    4    1   -2
##[3,]  -14   -3    7
