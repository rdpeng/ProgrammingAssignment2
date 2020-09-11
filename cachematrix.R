## Put comments here that give an overall description of what your
## functions do

## It is used to create a 'special' matrix and cache the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
   inve<-NULL
   set<-function(j){
     x<<-j
     inve<<-NULL
   }
   get<-function() x
   setInverse<-function(solveMatrix) inve<<-solveMatrix
   getInverse<-function() inve
   list(set=set,get=get,setInverse=setInverse,getInverse=getInverse)
}


## It is used to solve the inverse of the matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inve <- x$getInverse()
  if(!is.null(inve)){
    message("cached data available")
    return(inve)
  }
  data <- x$get()
  inve <- solve(data)
  x$setInverse(inve)
  inve      
}
