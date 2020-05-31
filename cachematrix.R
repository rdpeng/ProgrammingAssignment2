## The combination of this two functions take  a matrix and calculates the
## inverse, the result is stored in a "invisible" enviroment of an object, so
## if in the future 

## This function creates an object that store a matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {

      m<-NULL
      
      set<-function(y){
            x<<-y
            m<<-NULL
      }
      get<-function(){
            x
      }
      setinv<-function(inv){
            m<<-inv
      }
      getinv<-function(){
            m
      }
      list(set=set,
           get=get,
           setinv=setinv,
           getinv=getinv)
}


## This function takes the output from "makeCacheMatrix" , then it calculates
## and stores the inverse of the entered matrix. If it already exist, it will 
## not make the calculation and just take the result from the cache memory, 
## given you a message saying: "getting cached data".

cacheSolve <- function(x, ...) {
      m <- x$getinv()
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
      }
      data <- x$get()
      m <- solve(data)
      x$setinv(m)
      m
}
