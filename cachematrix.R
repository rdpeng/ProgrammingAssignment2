## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  invm <- NULL
  set <- function(y) {
    x <<- y
    invm <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) invm <<- inverse
  getinv <- function(inverse) invm
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)      

}

myMatrix <- makeCacheMatrix(matrix(1:4, 2,2))
myMatrix$get()
myMatrix$setinv()
myMatrix$getinv()


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
         invm <- x$getinv()
  if(!is.null(invm)) {
    message("getting cached data")
    return(invm)
  }
  matr <- x$get()
  invm <- solve(matr, ...)
  x$setinv(invm)
  invm
}
          
cacheSolve(myMatrix)
cacheSolve(myMatrix)

myMatrix$set(matrix(c(11,11,11,11), 2,2))
myMatrix$getinv()
myMatrix$setinv(222)
myMatrix$getinv()
