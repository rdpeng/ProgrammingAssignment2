## Assignment on Lexical scoping

## This function creates a special object matrix which cache the inverse of the input matrix.
## Note: The input matrix should be invertible.

makeCacheMatrix <- function(x = matrix()) {
m<-NULL
set<-function(y){
x<<-y
m<<-NULL
}
get<-function() x
setinverse<-function(value) m <<-value
getinverse<-function() m
list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


## This function computes the inverse of the special object matrix from the above function 
##  If the inverse has already been calculated then it will retrive the inverse from the above created cache
       
cacheSolve<-function(x,...){
       m<-x$getinverse()
if(!is.null(m)) {
message("getting cache data")
return(m)
}
data<-x$get()
m<-solve(data,...)
x$setinverse(m)
m

}
