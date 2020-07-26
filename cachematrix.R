
##there are two functions makecachematrix and cachesolve


##makecachematrix consist of Set Get setinverse getinverse


makecachematrix <- function(t = matrix()){
n <- NULL                               ##intialize inverse as NULL
Set <- function(u){
t <<- u
n <<- NULL
}
Get <- function()t                      ## function to get matrix t
setinverse <- function(inverse)n <<- inverse
getinverse <- function()n               ## function to obtain inverse of the matrix
list(Set=Set,
Get=Get,
setinverse=setinverse,
getinverse=getinverse)
}

## this is used to get cache data

cacheSolve <- function(t=matrix(),...)  ## gets cache data
{
n <- t$getinverse()
if(!is.null(n)){                        ## checking whether inverse is NULL
message("getting cached data")
return(n)                               ## return inverse value
}
data <- t$Get()
n <- solve(data,...)                    ##calculate inverse value
t$setinverse(n)
n                                       ## return a matrix that is inverse of t
}

