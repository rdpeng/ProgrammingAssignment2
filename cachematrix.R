## This function creates a special "matrix" object x
## containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) { 
  ## define the cache m and initialize it to null
  m <- NULL
  set <- function(y) {
    x <<- y 
    m <<- NULL ## re-initialize m in the parent environment to null
  }
  
  get <- function() x 
  ## set m equal to the inverse of the matrix x
  setinverse <- function(inverse) m <<- inverse 
  ## return the cached inverse of x
  getinverse <- function() m 
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function calculates the inverse of the special "matrix" created
## with the above function. If so, it 'get's the inverse from the cache
## and skips the computation. Otherwise, it calculates the matrix inverse
## and sets the value of the inverse in the cache via the 'setinverse' function.

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
  
  m <- x$getinverse()
  ## checks to see if the inverse is caclulated
  ## if yes, it gets the inverse of cache
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  ## else if computes and sets the value of inverse 
  ## by using set inverse function
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}