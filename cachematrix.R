## Peer graded assgnment 2

makeCachematrix <- function(x=matrix()){
        inv <-NULL
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
        get <- function() {x}
        setinverse <- function(inverse) {inv <<- inverse}
        getinverse <- function(){inv}
        list(set= set, get=get, setInverse=setinverse, getInverse= getinverse)
}
cachesolve <- function(x,...){
        inv <- x$getInverse()
        if(!is.null(inv)){
                message ("getting cache data")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat,...)
        x$setinverse(inv)
        inv
}