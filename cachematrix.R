## These functions are part of my code for the Week 3 prgramming assignment of 
##Coursera's Data Science R Programming course. I started this assignment on 
##the 3rd of January in the year of 2020; My GitHub username is VibhavProg

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  setMatrix <- function(y){
    x <<- y
    inverse <<- NULL
  }
  getMatrix <- function(){
    x
  }
  setInverse <- function(inverseTwo){
    inverse <<- inverseTwo
  }
  getInverse <- function(){
    inverse
  }
  list(setMatrix = setMatrix, getMatrix = getMatrix,setInverse = setInverse, getInverse = getInverse)
}



## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  inverse <- x$getInverse()
  if(!is.null(inverse)){
    return(inverse)
  }
  dat <- x$getMatrix()
  inverse <- solve(dat, ...)
  x$setInverse(inverse)
  inverse
}
