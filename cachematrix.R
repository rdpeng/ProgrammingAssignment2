## makeCacheMatrix function is defined initially.
## This function is defined to take the value of a matrix and compute it's Inverse using 'Solve'.
## setmatrix is used to set the Inverse
## getmatrix is used to get the Inverse. It can be used by the user to check the computed Inverse of the matrix.

makeCacheMatrix <- function(b = matrix()) {   
  m<-NULL
  set<-function(y){
  b<<-y
  m<<-NULL
}
get<-function() b
setmatrix<-function(solve) m<<- solve
getmatrix<-function() m
list(set=set, get=get,
   setmatrix=setmatrix,
   getmatrix=getmatrix)
}

## cacheSolve is a function which calculated the Inverse of a matrix provided the inverse is not calculated before.
## For the matrix whose inverse is calculated, it will get the inverse from CacheMatrix and would skip the computation.
## Else, it would calculate the Inverse using the 'solve'.  

cacheSolve <- function(b=matrix(), ...) {  
    m<-b$getmatrix()
    if(!is.null(m)){
      message("getting cached data")
      return(m)
    }
matrix <- b$get()
m <- solve(matrix, ...)
    x$setmatrix(m)
    m
}

##END
