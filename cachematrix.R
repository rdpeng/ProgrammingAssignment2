## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##  The makeCacheMatrix function creat a matrix that can cache the inverse
##  The main function include four fuctions:set() ,get(),setsolve(),getsolve()

makeCacheMatrix <- function(x = matrix()) {
     s<-NULL
     set<-function(y){
        x<<-y
        s<<-NULL
        }
        get<-function()x
        setsolve<-function(solve) s<<-solve
        getsolve<-function() s
        list(set=set,get=get,setsolve=setsolve,getsolve=getsolve)

}


## Write a short comment describing this function
##get the inverse from the cache if it  exists,otherwise calculate the inverse  
## and set the value in  the cache by setsolve function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        s<-x$getsolve()
        if(!is.null(s)){
          message("getting cached data")
          return (s)
          }
          data<-x$get()
          s<-solve(data)
          x$setsolve(s)
          s
      }
