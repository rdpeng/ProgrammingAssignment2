## Caching the Inverse of a Matrix
## Below is a pair of functions that are used to cache the inverse of a matrix

## This function creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        i<-NULL                                
        set<-function(y){
                x<<-y
                i<<-NULL
        }                                       
        get<-function()x                        
        setinverse<-function(solve)i<<-solve    
        getinverse<-function()i                 
        list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


## This function computes the inverse of the special matrix created by makeCacheMatrix.If the inverse is already calculated and the matrix has not changed, then it should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i<-x$getinverse()                       
        if(!is.null(i)){                        
                message("getting cached data")
                return(i)
        }
        data<-x$get()                           
        i<-solve(data,...)
        x$setinverse(i)                         
        i
}
