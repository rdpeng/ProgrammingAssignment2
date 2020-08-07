## A programme to calculate the inverse of a big matrix in short time by caching the already calculated inverses

## Cache the inverse of marices already calculated 

makeCacheMatrix <- function(x = matrix()) {
 i<-NULL
  set<-function(y){
    x<<-y
    i<<-NULL
  }
  get<-function() x
  setinverse<-function(inverse) i<<-inverse
  getinverse<-function() i
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)

}


## Check whether the inverse of the matrix is available in the cache; othrwise calculate the inverse

cacheSolve <- function(x, ...) {
       i<-x$getinverse()
  if(!is.null(i)){
    message("get cached data")
    return(i)
  }
  data<-x$get()
  i<-solve(data,...)
  x$setinverse(i)
  i
}
