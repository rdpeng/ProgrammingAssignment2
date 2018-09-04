## The pair of functions creates a special matrix and caches the inverse of it

## It creates a special matrix that caches its inverse

makeCacheMatrix <- function(x = matrix()) {inv<-NULL
set<-function(y){
  x<<-y
  inv<<-NULL
}
get<-function()x
setinverse<-function(inverse) inv<<-inverse
getinverse<-function() inv
list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)

}


## The function returns the cached inverse or calculates it on providing suitable matrix

cacheSolve <- function(x, ...) { inv<-x$getinverse()
if (!is.null(inv)){
  message("getting cached data")
  return(inv)}
matr<-x$get()
inv<-solve(matr,...)
x$setinverse(inv)
inv
        ## Return a matrix that is the inverse of 'x'
}
