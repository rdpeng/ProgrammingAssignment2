## The program is going to check whether the inverse of a given matrix has been 
## already calculated. If so, it will get the inverse of matrix from 
## of the environment instead calculating it again.

## The function makeChacheMatrix creates a special object which contains 
## a list which consists of 4 functions: set the matrix, get the matrix, 
## get the inverse of matrix, set the inverse of matrix.

makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL
  setMatrix<-function(mat){
    x<<-mat
    inv<<-NULL
  }
  getMatrix<-function() x
  setInverse<-function(inverse) inv<<-inverse
  getInverse<- function() inv
  matList<-list(set=setMatrix,get=getMatrix,setInverse=setInverse,getInverse=getInverse)
  return(matList)
}


## The functon cacheSolve calculates the inverse of a given matrix, but first it 
## checks if it has not been calculated already. If so, it gets the inverse from
## cache (getInverse) and skips the computation. Otherwise, it calculates the 
## inverse of the  matrix and sets the value of the inverse in the cache via 
## the setInverse function.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  result<-x$getInverse()
  if(is.null(result)){
    message("calculating the inverse of matrix")
    data<-x$get()
    inv<-solve(data,...)
    x$setInverse(inv)
    result<-x$getInverse()
  }
  return (result)  
}