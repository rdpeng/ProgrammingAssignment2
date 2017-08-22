## Put comments here that give an overall description of what your
## functions do
## This function cache the the inverse of matrix during the first step and store the inverse matrix in the enviroment for later pulling

## The code is directly modified from the makeVector example, this step stores the inverted matrix in the enviroment

makeCacheMatrix<-function(x=matrix()){
    invert<-NULL
    set<-function(y){
        x<<-y
        invert<<-NULL
    }
    get<-function()x
    setInverse<-function(inverse)invert<<-inverse
    getInverse<-function() invert
    list(set=set,
         get=get,
         setInverse=setInverse,
         getInverse=getInverse)
}

# The second function can use matrix cached from the first step. 

cacheSolve<-function(x,...){
    invert<-x$getInverse()
    if(!is.null(invert)){
        message("getting cached data")
        return(invert)
    }
    mat<-x$get()
    invert<-solve(mat,...)
    x$setInverse(invert)
    invert
}

