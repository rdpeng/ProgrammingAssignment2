## These two functions allow us to cache the inverse of a matrix
## so it does not take a lot of time to get it (recalculating the inverse
## every single time could take a long time)

## This function is used to create the matrix

makeCacheMatrix <- function(x = matrix()) {
    inv<-NULL ## If we haven't calculated the inverse, its value is going to be NULL
    set<-function(y){
        x<<-y
        inv<<-NULL
    }
    get<-function(){
        x
    }
    setinverse<-function(inverse){
        inv<<-inverse
    }
    getinverse<-function(){
        inv
    }
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## This function returns the inverse of the matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv<-x$getinverse()
    if(!is.null(inv)){
        message("searching cached data") ## If we have already calculated the inverse
        return(inv)
    }
    data<-x$get()
    inv<-solve(data, ...) ## solve() allow us to calculate the inverse of a matrix
    x$setinverse(inv)
    inv ## The output is the inverse of our "x" matrix
}
