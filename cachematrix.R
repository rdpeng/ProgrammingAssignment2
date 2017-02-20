## The below functions returns the matrix inverse and then caches that inverse


## The makeCacheMatrix function takes a matrix as a parameter, and returns a special "matrix", which is really a list containing a function to
#set the value of the matrix
#get the value of the matrix
#set the value of the matrix inverse
#get the value of the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(matrixinverse) m <<- matrixinverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## The cacheSOlve function calculates the mean of the special "matrix" created by makeCacheMatrix.
#However, it first checks to see if the matrix inverse has already been calculated. 
#If so, it gets the matrix inverse from the cache and skips the computation. 
#Otherwise, it calculates the matrix inverse of the data and sets the value of the inverse in the cache 
#via the setinverse function.    

cacheSolve <- function(x) {
        ## Return a matrix that is the inverse of 'x'
   m <- x$getinverse()
   if(!is.null(m)) {
      message("getting cached data")
     return(m)
   }
   data <- x$get()
   m <- solve(data)
   x$setinverse(m)
   m
   
}
