## functions do
##makeCachematrix creates the matrix and cacheSolve calculates the inverse of the matrix


##makeCachematrix function has four functions-
##1)set the value of matrix
##2)get the values of matrix
##3)set the value of inverse
##4)get the value of inverse

makeCachematrix <- function(x = numeric(),y= numeric()) {
  i <- NULL
  mat<-rbind(x,y)
  set <- function(x,y) {
    x<<-x
    y<<-y
    mat1<-rbind(x,y)
    mat<<-mat1
    i <<- NULL
  }
  get <- function() mat
  setinverse <- function(x2=numeric(),y2=numeric()) i <<- solve(rbind(x2,y2))
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}



## cacheSolve calculates the inverse of the matrix if it is not found already.

cacheSolve <- function(mat, ...) {
  i <- mat$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- mat$get()
  i <- solve(data, ...)
  mat$setinverse(i)
  i
}
