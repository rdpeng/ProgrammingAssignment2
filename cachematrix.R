## Put comments here that give an overall description of what your
## functions do

## This firs function creates a cache matrix. A list with function as elementes that will allow later to obtain the invers of the matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y){
                x<<-y
                inv<<-NULL
        }
        get <- function() x
        setmean <- function(solve) inv<<-solve
        getmean<-function() inv
        list(set=set,get=get,setmean=setmean,getmean=getmean)
}

## This second function get de inverse matrix
cacheSolve <- function(x, ...) {
        inv<-x$getmean()
        if(!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        data<-x$get()
        inv<-solve(data,...)
        x$setmean(inv)
        inv
}
