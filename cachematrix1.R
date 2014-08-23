## In this assignment, I first create an object to store the inverse matrix 
## of a certain matrix calculated in another function, cacheSolve().
##  

## makeCacheMatrix creates an "object" of type ‘list’, including:set,
## get, setinverse, and get inverse.

makeCacheMatrix <- function(x = matrix()) {#input x is a matrix.
  m<-NULL
  
  set<-function(y){
    x <<- y
    m <<- NULL
  }
  # get returns the value of the original matrix x.
  get <- function()x
  # setinverse is called by cacheSolve and it will store the calculated inverse
  # matrix.
  setinverse <- function(inverse) m <<- inverse
  # getinverse would return the cached value
  getinverse <- function() m
  list(set = set, get = get, 
       setinverse = setinverse,
       getinverse = getinverse)

}

## cacheSolve() accesses the object -- if the inverse matrix has not yet been
## calculated, cacheSolve() calculates it and stores it in makeCacheMatrix().
## If the inverse matrix has been calculated earlier then cacheSolve() simply 
## fetches it and returns the inverse matrix, saving the computing time required
## to calculate the inverse matrix again. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data<-x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}

a<-matrix(c(1,-1/4,-1/4,1),2,2)
b<-makeCacheMatrix(a)
cacheSolve(b)
