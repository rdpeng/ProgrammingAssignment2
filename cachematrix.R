

## Function makeCacheMatrix make matrix creates a special vector which is really
## a list containing functions to the things below:
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse
## get the value of the inverse
## We can use it to make a Matrix which will cache its inverse. 

makeCacheMatrix <- function(x = matrix()) {
    m<-NULL
    set<-function(y){
        x<<-y
        m<<-NULL
    }
    get<-function(){
        x
    }
    setinverse<-function(inverse){
        m<<-inverse
    }
    getinverse<-function(){
        m
    }
    list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


## function cacheSolve will return the inverse matrix of x,which was created by 
## makeCacheMatrix.The function will firstly check whether the inverse had been 
## caculated before .If it had been caculated,the function will get the result
## just from the memory.If not,it will caculate the inverse and store it in mem.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m<-x$getinverse()
    if(!is.null(m)){
        message("getting cached data")
        return (m)
    }
    data<-x$get()
    m<-solve(data)
    x$setinverse(m)
    m
}
