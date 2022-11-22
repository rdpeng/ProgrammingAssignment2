## Functions to create matraix and compute its inverse matrix
## clean the working space
ls()
rm(list=ls())

## Create a function called makeCacheMatrix to create matraix
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inversion
## 4. get the value of the inversion

## set x as default value
makeCacheMatrix <- function(x = matrix(c(1,2,3,4,5, 11,12,13,14), nrow = 3, ncol = 3,)) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinver <- function(solve) m <<- solve
  getinver <- function() m
  list(set = set, get = get,
       setinver = setinver,
       getinver = getinver)
}


## Create a function called cacheSolve to compute inverse matraix
## x is the returned matrix by calling makeCacheMatrix 
cacheSolve <- function(x, ...) {
  m <- x$getinver()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setinver(m)
  m
  ## Return a matrix that is the inverse of 'x'
}

## Test and validate the functions
x <- makeCacheMatrix(); x
data <- x$get(); data
data1 <-x$getinver(); data1
inver <- cacheSolve(x); inver
chk <- round(data %*% inver,3); chk
