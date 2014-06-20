## Put comments here that give an overall description of what your
## functions do

## This function creates a special "vector", which is really a list of four functions, used to create an object that stores a matrix and it's inverse.
## This list of functions is composed of:
## 1. A function to set the value of the matrix
## 2. A function to retrieve (get) the matrix
## 3. A function to set the value of the inverse matrix
## 4. A function to retrieve (get) the value of the inverse matrix

makeCacheMatrix <- function(x = matrix(), nrow, ncol){
  m <- NULL
  set <- function(y, nrow, ncol) {
    x <<- matrix(y, nrow, ncol)
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function calculates the inverse of the matrix created with the makeCacheMatrix() function and sets the value of the inverse matrix in the cache via the setinverse function,
## but firstlly it checks if the inverse of that matrix has already been calculated
## and if so, it gets the inverse from the cache and skips the computation.

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}

## So if I call the makeCacheMatrix() function,
## I assign the list of functions to the variable a,
## so a is now a list of four functions.

a <- makeCacheMatrix()

## I can use the a's set function to create an invertible matrix with the numbers 2, 5, 1, 3 and dimensions 2x2

a$set(c(2,5,1,3),2,2)

## Then I can use the a's get function to get the matrix created

a$get()

## If I pass the list a to the cacheSolve() function
## This will compute and return the inverse of the matrix, because the inverse was not cached yet

cacheSolve(a)

## If I pass the list a to the cacheSolve() function a scond time, the value of the inverse matrix should be returned,
## but also a message that the inverse is not being calculated because is being retrieved from the cache

cacheSolve(a)

## If I use the a's set function to create a new matrix with the numbers 2, 4, 2, -2 and dimensions 2x2

a$set(c(2,4,2,-2),2,2)

## Then, when I pass the list a to the cacheSolve() function, the inverse matrix should be computed and returned again

cacheSolve(a)

## Finally If I pass the list a to the cacheSolve() function again, the value of the inverse matrix should retrieved from the cache,
## and also a message that the inverse is not being calculated this time

cacheSolve(a)
