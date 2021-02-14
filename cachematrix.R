## Put comments here that give an overall description of what your
## functions do

## This function cashes the inverse matrix rather than compute it repeatedly

makeCacheMatrix <- function(x=matrix()){
        inv <- NULL
        set <- function(){
                x <<- y
                inv<<- NULL
                
        }
        obtain <- function(){x}
        setInverse <- function(inverse){inv<<- inverse}
        obtainInverse <- function(){inv}
        list(set=set, obtain=obtain, setInverse=setInverse, obtainInverse = obtainInverse)
}

cacheSolve <- function(x,...){
        inv <- x$obtainInverse()
        if(!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        mat <- x$obtain()
        inv <- solve(mat,...)
        x$setInverse(inv)
        inv
}
