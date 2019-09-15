## Put comments here that give an overall description of what your
## functions do

## The objective is to create functions that dont have to calculate the inverse matrix of an input if it is
## the same input matrix. The idea is to calculate and store the inverse in a variable and use lexical scoping of R to 
## call, compare and compute the inverse of a given matrix

## The first function is makeCacheMatrix which is to initialize the variables with objects (here x and m)
## set the initial values (m==Null and x=given matrix)

## The second funtion is cacheSolve which will calculate the inverse and stores the value in m

## Write a short comment describing this function
## makeCacheMatrix function creates a matrix in cache using list of functions set, get, setinverse & getinverse. 
## the set function assigns the value to x and sets the value of m to Null to clear prior values if exists
## the get function gets the input value
## setinverse funtion assigns the value of input argument to m
## getinverse funtion gets the value of new assigned value of m

## last statement of list is to creat the list out of above functions so that it can be used later in the second funtion

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y=matrix()) {
    x <<- y
    m <<- NULL
  }
  
  get <- function() x
  
  setinverse <- function(inverse) m <<- inverse
  
  getinverse <- function() m
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function

## The function CacheSolve first tries to check if m has a value using the getinverse() function. 
## since the inital value of m in first funtion was set to Null, we use 'if' condition to determine if 
## input matrix is a new matrix or not

## If m is not null, then it gets the value from cache and returns m 
## else it gets the matrix x using get function and calculates the inverse of X 
## the setinverse function then stores the inverse matrix in the cache

cacheSolve <- function(x, ...) {
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
