## I want to create 2 functions: 1) retrieve the inverse matrix from the cache 2)  calculate the inverse matrix
## functions do

##  "makeCacheMatrix" should create  "matrix" object that to cache to its inverse.
##  makeCacheMatrix contains 4 functions: set, get, setmean, getmean 
## Used the principle from Caching the Mean of a Vector

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  
  setInverse <- function(solve) m <<- solve
  getInverse <- function() m
  
  list(set = set, get = get,setInverse = setInverse,getInverse = getInverse)
}


##123456

##  "cacheSolve" RETURNS  a matrix that is the inverse of 'x'
##  returned by makeCacheMatrix.

 cacheSolve <- function(x, ...) {

  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m}

       
##Output
##> a
##      [,1] [,2] [,3]
##[1,]    4    0    0
##[2,]    0    4    0
##[3,]    0    0    4
##> m2<- makeCacheMatrix(a)
##> cacheSolve(m2)
##     [,1] [,2] [,3]
##[1,] 0.25 0.00 0.00
##[2,] 0.00 0.25 0.00
##[3,] 0.00 0.00 0.25
>
