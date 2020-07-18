## makeCacheMatrix->This function creates a special "matrix" object that can cache its inverse.
## cacheSolve-> This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

## Computing the inverse of a square matrix can be done with the solve function in R

makeCacheMatrix <- function(x = matrix()) {
inv<-NULL
set<-function(y){
x<<-y
inv<<-NULL
}
get<-function(){x}
setInverse<-function(inverse){inv<<-inverse}
getInverse<-function(){inv}
list(set=set,gst=get,setInverse=setInverse,getInverse=getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
inv<-x$getInverse()
if(is.null(inv)){
message("Getting cached")
return(inv)
}
mat<-x$get()
inv<-solve(mat,...)
x$setInverse(inv)
inv

        ## Return a matrix that is the inverse of 'x'
}
