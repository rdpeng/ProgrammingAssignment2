## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

##The 'makeCacheMatrix' function creates a special "matrix" object that can cache its inverse.


makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
  set <- function(y) {      #set the value of the vector
          x <<- y
          i <<- NULL
  }
  get <- function() x        #get the value of the vector
  setinverse <- function(inverse) i <<- inverse     #set the value of the inverse
  getinverse <- function() i        #get the value of the inverse
  list(set = set,                   #create a list
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function

##The cacheSolve function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), 
##then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
         i <- x$getinverse()
  if (!is.null(i)) {                        ##Checks if the value of 'i' is not null and returns 'i'
          message("getting cached data")
          return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
