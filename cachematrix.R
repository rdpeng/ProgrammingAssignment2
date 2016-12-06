##functions cache the inverse of a matrix 


##function creates a special matrix object that caches its inverse 


makeCacheMatrix <- function(x = matrix()) {
  m<-NULL # Initialise m
  set<-function(y){ 
    x<<-y 
    m<<-NULL 
}
get<-function() x 
setmatrix<-function(solve) m<<- solve 
getmatrix<-function() m
list(set=set, get=get,
     setmatrix=setmatrix,
     getmatrix=getmatrix) 
}
     
## function computes the inverse of the special matrix returned by makeCacheMatrix above 

cacheSolve <- function(x, ...) {
  m<-x$getmatrix() 
  if(!is.null(m)){ 
    message("getting cached data") 
    return(m) 
}
matrix<-x$get() 
m<-solve(matrix, ...) 
x$setmatrix(m) 
m 
}
     
