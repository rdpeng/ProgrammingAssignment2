## There are two functions namely makeCacheMatrix and cacheSolve 
## makeCacheMatrix consist of set,get,setinv,getinv. this function takes matrix as input and calculates its inverse

## the loading of MASS is used to calculate the inverse of the matrix
##the inverse that is found is cached.
library(MASS)
makeCacheMatrix <- function(x = matrix()) {
                inv<-NULL            
                set<-function(y){
                                 x<<-y
                                 inv<-NULL
                                  }
                get<-function()x              
                setinv<-function(inverse)inv<<-inverse
                getinv<-function(){ 
                                   inver<-ginv(x)
                                   inver%*%x   
                                   }
                list(set = set, get = get, 
                     setinv = setinv, 
                     getinv = getinv)
}

 ## Return a matrix that is the inverse of 'x'
## cache solve checks the inverse value inv whether it is null. if the value is not null its returns the cached value
cacheSolve <- function(x, ...) {
                              inv<-x$getinv()                  
                              if(!is.null(inv)){                 
                              message("getting cached data!")
                              return(inv)                       
                               }
                    data<-x$get()
                    inv<-solve(data,...)             
                    x$setinv(inv)
      }
