## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x) { 
  inverse <- NULL
  set <- function(y){
    x <<- y
    inverse <<- NULL
  }
  get <- function() { x}

  setInverse <- function(iMat) {
    inverse <<- iMat
  }
  getInverse <- function() inverse
  list(set = set, get = get, getInverse= getInverse, setInverse = setInverse)
}

