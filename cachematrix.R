

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

## Write a short comment describing this function

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
