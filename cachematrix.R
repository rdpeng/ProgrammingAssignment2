## This function creates cache

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


## This function returns the inverse of x

cacheSolve <- function(x=matrix()) {
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
