## This file contains an example of how to use the <<- operator.
##The example is new datatype that cache the calculated inverse of a matrix in 
##the datatype itself
##It has two following functions
##1. makeCacheMatrix: Generate a new cacheable matrix type out of a regular matrix
##2. solveMatrix:Generate inverse of matrix based on solve ,taking cached result
##if already calcuated
##Example:
# size = 10
## A <- matrix(runif (size * size), ncol = size)
## a <- makeCacheMatrix(A)
## a$getinverse ()
## check if inverse is empty or NULL
## cacheSolve (a)
## B <- matrix (runif (size*size),ncol = size)
## b <- makeCacheMatrix(B)
## cacheSolve (b)
## Validate that a, b are not identical(therefore different)
## sum(a$get()!= b$get()) >=0
## TRUE
## sum(a$getinverse()!= b$etinverse()) >=0
##TRUE


##This functions creates a special type of matrix which is able to cache an inverse next to the matrix itself, as a new datatype
## It binds four functions to the datatype and two internal variables i and x
makeCacheMatrix <- function(x = matrix()){
  ## reset internal variables x and i
  i= null
set <- function (y){
  ## set the matrix value to internal var x (and reset the inverse)
x <<- y
i<<- NULL
}
get <- function()x ## get the matrix value from internal var x
setinverse <- function (inverse) i <<-inverse #set the inverse i (next to the matrix value)
getinverse <- function() i ## get the inverse i(cached next to the matrix value)
list ( set = set, get = get, setinverse = setinverse, getinverse = getinverse)
## binds four functions to this datatype
}

## This function generates the inverse of a matrix of our cacheable matrix type
## If inverse was already calculated, it returns the cached result,thereby saving cpu cycles, else calculate and save it

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
        i<- x$getinverse()
        if (!is.null(i){
        message("getting cached data")
        return (i)
          # it was cached, so do nothing ,just return value
        }
        # if it is not yet calculated and cached
        data <- x$get()
        i<- solve(data,...)
        x$setinverse(i)#cache
        i # and return inverse
        }
}
