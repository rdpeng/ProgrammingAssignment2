# R-language
makeCacheMatrix <- function(x=matrix(),...){ 
  ##This function creates a special "matrix" object that can cache its inverse
  m<-NULL
  set<-function(y){
    x<<-y 
    m<<-NULL
  }
  get<-function() x
  setM<-function(solve) m<<-solve
  getM<-function() m
  list(set=set,get=get,setM=setM,getM=getM)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  ##This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
  m <- x$getM()
  if(!is.null(m)){
    message("TO cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data,...)
  x$setM(m)
  m
}
