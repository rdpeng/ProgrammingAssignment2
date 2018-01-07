## Calculate and cashe the inverse of the matrix.

## The function makeCacheMatrix() set the value and get the value of the matrix, 
## and also set the value and get the value of the inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
  inver<-NULL
  set<-function(y){
    x<<-y
    inver<<-NULL
  }
  get<-function() x
  setinverse<-function(inverse) inver<<-inverse
  getinverse<-function() inver
  list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


## The function return calculate the inverse of the matrix in the above function.

cacheSolve <- function(x, ...) {
        inver<-x$getinverse()
        if(!is.null(inver)){
          message("getting cached data")
          return(inver)
        }
        data<-x$get()
        inver<-solve(data)
        x$setinverse(inver)
        inver
}

