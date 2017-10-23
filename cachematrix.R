## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(mymatr = matrix()) {
  m<-NULL
  set<-function(y){
      mymatr<<-y
      m<<-NULL
  }
  get <- function() mymatr
  setinverse <- function(invMatr) {m <<- invMatr
  }
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)     

}


## Write a short comment describing this function

cacheSolve <- function(mymatr, ...) {
  m<-mymatr$getinverse
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- mymatr$get()
  m <- solve(data, ...)
  mymatr$setinverse(m)
  m
        ## Return a matrix that is the inverse of 'mymatr'
}


