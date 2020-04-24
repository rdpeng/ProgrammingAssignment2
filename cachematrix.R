## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function


makeCacheMatrix <- function(x = matrix()) {
  ## get value of the matrix
  m <- NULL
  
  ## set the matrix
  set <- function(y)
    x <<- y
  m <<- NULL
}

## get the matrix and return it
get <- function() x

## set the inverse
setInverse <- function(inverse) m <<- inverse

## get the inverse
getInverse <- function() m

## return list
list(set=set, get=get,
     setInverse=setInverse,
     getInverse=getInverse)


## creating a matrix that can cache the inverse

## Write a short comment describing this function

cacheSolve <- function(x, ...) 
  
  
  ## return inverse of x
  m <- x$getInverse

## return the inverse
if(!is.null(m)) {
  message("getting inverse matrix")
  return(m)
}

## get matrix
get <- x$get()

## calculate the inverse
m <- solve(data, ...)

## set inverse
x$setInverse(m)

## return matrix

m
