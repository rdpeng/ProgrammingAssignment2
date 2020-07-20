
    makecachematrix <- function(t = matrix()){
n <- NULL
Set <- function(u){
t <<- u
n <<- NULL
}
Get <- function()t
setinverse <- function(inverse)n <<- inverse
getinverse <- function()n
list(Set=Set,Get=Get,setinverse=setinverse,getinverse=getinverse)
}



cacheSolve <- function(t=matrix(),...){
n <- t$getinverse()
if(!is.null(n)){
message("getting cached data")
return(n)
}
data <- t$Get()
n <- solve(data,...)
t$setinverse(n)
n
}
  
