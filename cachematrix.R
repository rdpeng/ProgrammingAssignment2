# This is a Assignment for R Programming
# 06/05/2018 by jerebay (Holger)
# Two functions as below decribed
# this makeCacheMatrix function takes any sqaure matrix as
# an argument and calculates inverse of square matrix using solve function

makeCacheMatrix<-function(x=matrix()){
  matrixInverseCopy<-NULL      # initialized matrixInverseCopy with NULL value
  set<-function(y){
    x <<- y                     # and assign value of matrix x for example
    matrixInverseCopy <<- NULL         
  }
  get <- function() x        # and get value of matrix x
  
  # setInverse function calcualtes the inverse of matrix for x and assign the result
  # to matrixInverseCopy
  setInverse <- function (solve) matrixInverseCopy <<- solve
  # getInverse function is used to get inverse of matrix x
  getInverse <- function() matrixInverseCopy
  
  list(set=set,get=get,setInverse=setInverse,getInverse=getInverse)
}

# The cacheSolve function takes matrix as a argument and checks whether  the matrix inverse
# is calculated by makeCacheMatrix function. In case the calculated than it
# print message by returning a inverse of matrix else calculate inverse of matrix x and return the result

cacheSolve<-function(x, ...){
  matrixInverse <- x$getInverse()  # inverse retrieve matrix 
  if(!is.null(matrixInverse)){   # in case the inverse is calculated
    message("Getting the cached data: Matrix inverse is already calculated.")
    return(matrixInverse) # Returning a matrix that is the inverse of x
  }
  data <- x$get()      #  to Retrieve a matrix using get method
  matrixInverse <- solve(data, ...)  # and Calculate inverse of matrix
  x$setInverse(matrixInverse) # to Set value of inverse of matrix
  (matrixInverse)  # and finally return a matrix that is the inverse of x
}