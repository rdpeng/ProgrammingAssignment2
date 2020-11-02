## I have made some changes but matrix inverse is not get.
## Matrix is fetched.
## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv<- NULL
  set<- function(y){
   x<<- y
   inv <<- NULL
}
 get<- function() {x}
setInverse<- function(inverse) {inv<<- inverse}
getInverse<- function() {inv}
list(Set=set, get=get, SetInverse=setInverse, getInverse=getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
       inv<- x$getInverse
       if(!is.null(inv)){
        message("get the cache data")
        return(inv)
        }
       mat<- x$get()
       inv<- solve(mat, ...)
       x$setInverse(inv)
}
kmatrix<- makeCacheMatrix(matrix(1:4, nrow=2, ncol=2)) 