#putting some random comment here
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() 
    x
  setInverse <- function(solve)
    inv <<- solve
  getInverse <- function() inv
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
 }

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached inverse matrix")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setInverse(inv)
  inv
}

testData <- c(1,2,3,4,6,7,8,9,10)
testMatrix = matrix(T1,nrow=3,byrow=T)

y <- testMatrix
x <- makeCacheMatrix(y)
cacheSolve(x)
