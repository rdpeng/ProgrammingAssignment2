## Our goal is to create a programe which be able to cache the inverse of a matrix.
## In order to do that we need to write two functions:
## 1.-makecacheMatrix will create a "special" Matrix that will cache the inverse
## 2.-cacheSolve will take the "special" Matrix stored in makecacheMatrix as input an will calculate its inverse.

## This method is analog to the example function makeVector(x) explained before the assignment.
## makecacheMatrix take as input a matrix (that we have to define in the global enviornment)
## It contains 4 functions. On one hand set and get with which we can set and get the input matrix (through object$set/get respectively)
## On the other hand setinv and getinve with which we can set and get the inverse of the matrix.

makecacheMatrix <- function(x=matrix()) {
  
  inv<-NULL 
  
  set <- function(y) {
    x <<- y
    
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## This method is analog to the example function cachemean(x) explained before the assignment.
## cacheSolve take as input the output of makecacheMatrix(x), a 'Special' matrix.
## At the begining inv would be NULL, which means that the matrix is not cached.
## The program will calculate the inverse of the 'Special' matrix via the function solve(data),
## previously having stored the matrix with the function get() in makecacheMatrix. And it return the value.
## If you run call cacheSolve twice you will see a message in the screen saying "getting cached data" which 
## means that the inverse of the matrix is already cached

cacheSolve <- function(x , ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv<- solve(data, ...)
  x$setinv(inv)
  inv
}
