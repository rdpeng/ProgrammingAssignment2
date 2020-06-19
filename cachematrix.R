## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
 inv <- NULL
 set <- function(y){     ##Set value of matrix
   x<<- y                 ## <<- capable of modifying operator in all levels
   inv <<- NULL
 }

get <- function() {x} ##get value of matrix
setInverse <- function(inverse) {inv <<- inverse} ## set value of inverse matrix
  getInverse <- function() {inv} ## get value of inverse matrix
 list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
 }

cachesolve <- function (x, ...){
  inv <- x$getInverse()
  if(!is.null(inv)){            ## check inverse if already calculated
    message("getting cache data")
    return(inv)                ## inverse to be calculated if not in cache
  }
mat <- x$get()
inv <- solve(mat, ...)
x$setInverse(inv)
  }



