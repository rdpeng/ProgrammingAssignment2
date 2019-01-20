## Setting value of the matrix

## applying function to inverse the matrix

makeCacheMatrix <- function(x = matrix()) {
m<- NULL
set<- function(y){
        x<<-y
        m<<-NULL
}
get <-function()x
setinverse<-function(inverse) m<<-inverse
getinverse<-function()m
list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## retrieving the inverse from cache list

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m<-x$getinverse()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        data<- x$get()
        m<-solve(data, ...)
        x$setinverse(m)
        m
        }
