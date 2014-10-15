makeCacheMatrix<-function(x=matrix()){
m <- NULL
set <- function(y) {
x <<- y
m <<- NULL
}
get<-function()x
setsolve<-function(solve) m<<-solve
getsolve<-function() m
list(set=set,get=get,
setsolve=setsolve,
getsolve=getsolve)
}

cachesolve <- function(x) {
m <- x$getsolve()
if(!is.null(m)) {
return(m)
}
data <- x$get()
m <- solve(data)
x$setsolve(m)
m
}
