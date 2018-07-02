
#This method creates 2 assignment and 2 access functions for the matrix, one of each for the matrix itself and its inverse.
#This method turns a list of the functions related to that matrix 
makeCacheMatrix <- function(x = matrix()) 
  {
  inverse <- NULL
  assign <- function(s) {
    x <<- s
    inverse <<- NULL
  }
  get <- function() {
    print(x)
  }
  
  assign.inverse <- function(input){
    inverse <<- input
  }
  get.inverse <- function(){
    print(inverse)
  }
  list(assign = assign, get = get,
       assign.inverse = assign.inverse,
       get.inverse = get.inverse)
}

#This method first checks to see if there is an inverse for the matrix.
#If there si not, it creates it inverse and then prints it, else it just prints it.

cacheSolve <- function(x, ...) {
  inverse <- x$get.inverse()
  if(is.null(inverse)){
    matrix <- x$get()
    inverse <- solve(matrix)
    x$assign.inverse(inverse)
  }else{
    return(inverse())
  }
  print(inverse)
}

#test


q<-matrix(c(2,0,0,1),2,2)
qz<-makeCacheMatrix(q)
cacheSolve(qz)
