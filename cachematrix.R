## Cache the inverse of matrix
## create a matrix & cache the inverse

## creates a matrix that can be inversed

makeCacheMatrix <- function(x = matrix()) {
I <- NULL
  set <- function(y){
    x <<- y
    I <<- NULL
  }
  get <- function(x)
  setInverse <- function(solve) 
    I <<- solve
  getInverse <- function(I) 
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}


## calculates & returns the inverse of a matrxix 

cacheSolve <- function(x, ...)
{
        x <- x$getInverse() ## Return a matrix that is the inverse of 'x'
+   if(!Is.null(x))
    {
+     message("getting cached data")
+     return(x)
    }
+   mat <- x$get()
+   x <- solve(mat)
+   x$setInverse(x)
+   x
}
