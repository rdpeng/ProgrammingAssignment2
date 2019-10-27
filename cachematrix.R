## Assignment: Caching the Inverse of a Matrix
## This is a pair of functions that cache the inverse of a matrix.

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x=matrix(),...){
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


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        m <- x$getM()
  if(!is.null(m)){
    message("TO cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data,...)
  x$setM(m)
  m
        ## Return a matrix that is the inverse of 'x'
}
