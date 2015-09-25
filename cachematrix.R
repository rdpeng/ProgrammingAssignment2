##https://github.com/rdpeng/ProgrammingAssignment2
##most recent commit SHA-1 hash: 7f657dd22ac20d22698c53b23f0057e1a12c09b7

##Peer Assessments /Programming Assignment 2: Lexical Scoping **Please Note: No Grace Period**
## Below are two functions that are used to create a special object that stores a matrix and cache's its inverse.

##The first function, makeCacheMatrix creates a special "matrix", which is really a list containing a function to
##1. set the value of the matrix
##2. get the value of the matrix
##3. set the value of the inverse
##4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        m<-NULL
        set<-function(y){
                x<<-y
                m<<-NULL
        }
        get<-function()x
        setinverse<-function(inverse) m<<-inverse
        getinverse<-function()m
        list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}

## The following function calculates the inverse of the special "matrix" created with the above function. However, 
## it first checks to see if the inverse has already been calculated. If so, it gets the inverse from the cache and 
## skips the computation. Otherwise, it calculates the inverse of the matrix and sets the value of the inverse in the 
## cache via the setmean function.

cacheSolve <- function(x, ...) {
        m<-x$getinverse()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        
        data<-x$get()
        m<-solve(data,...)
        x$setinverse(m)
        m
}
