## Put comments here that give an overall description of what your
## functions do

## Simply set x as the input matrix.
## Solve "inv" as a NULL.
## Instead of using other variables asides from the text, just use it as it is. 
## One can also change if wanted to. 

## Write a short comment describing this function

## makeCacheMatrix consists of set, get, setinv, and getiv

makeCacheMatrix <- function(x = matrix()){
    inv <- NULL ## This is used to initialie the inverse as NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() {x} ## This is used to get the function of matrix x
    setInverse <- fucntion(inverse) {inv <<- inverse}
    getInverse <- function() {inv}
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

##Write a short comment describing this function

## Do remember the variables set in every line.

## Cached data are the result og using this function.

cacheSolve <- function(x, ...){ ## Cached data will be the result
    
    ## Return the matrix that is the inverse of 'x'
    
    inv <- x$getInverse()
    if(!is.null(inv)) { ## This checks whether the inverse is NULL
      message("getting cached data")
      return(inv) ## The inverse value will be returned
    }
    mat <- x$get()
    inv <- solve(mat, ...)  ## USed to solve the inverse value
    x$setInverse(inv)
    inv  ## The matrix of the inverse of 'x' will be returned
}
