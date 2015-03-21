## The method makeCacheMatrix assigns a matrix to the function using set() function
## get() member of this function returns the matrix stored using set()
## setInverse() member function is used to set the Inverse of the matrix 
## getInverse() member function returns the value stored using setInverse()
## 

makeCacheMatrix <- function(x = matrix()){
   m <- NULL
   set <- function(y) {
     x <<- y
     m <<- NULL
   }
   get <- function() x
   setInverse <- function(solve) m <<- solve
   getInverse <- function() m
   list(set = set, get = get,
        setInverse = setInverse,
        getInverse = getInverse)
  
}

## cacheSolve() function takes the object created by makeCacheMatrix() as argument
## This method returns the inverse of the matrix object received through argument
## First, it checks whether the inverse has been calculated and stored 
## if store, returns that value.
## else calculates and stores that value (in line number 35)
## TThe folloing function returns the inverse as the last line says so.

cacheSolve <- function(x,...){
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data,...)
  x$setInverse(m)
  m
}
