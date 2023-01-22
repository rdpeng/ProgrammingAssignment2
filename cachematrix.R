## This function creates a data structure to store the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
  #initialize the inverse of the matrix to null
  InvMat <- NULL
  set<-function(y){
    #this function sets the value of y to x outside the scope by using << operator
    x <<- y
    InvMat <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) invMat<<-inverse
  getinverse <- function() invMat
  #return a list with all this
  list(set=set, get=get, 
       setinverse=setinverse, 
       getinverse=getinverse)
}

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  ## assuming x is already created using makeCacheMatrix
  
  ## first check if the matrix inverse has been calculated already
  InvMat<-x$getinverse()
  if(!is.null(InvMat)){
    print("Returning the cached version of the inverse")
    return(InvMat)
  }
  mat<-x$get()
  InvMat<-solve(mat)
  #store the value of InvMat
  x$setinverse(InvMat)
  InvMat
}
