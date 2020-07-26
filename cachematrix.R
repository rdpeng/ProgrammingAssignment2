## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix(),...) {
        i<-NULL
        set<-function(y){
                x<<-y
                i<<-NULL
        }
        get<-function()x
        setMinv<-function(solve)
        i<<-solve
        getMinv<-function()i
        list(set=set,get=get,setMinv=setMinv,getMinv=getMinv)
}
        
        



## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i<-x$getMinv()
        if(!is.null(i)){
        message("minv cached data")
        return(i)
        }
        data<-x$get()
        i<-solve(data,... )
        x$setMinv(i)
        i
}

