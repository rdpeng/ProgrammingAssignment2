## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##There are two functions makeCacheMatrix and makeCacheMatrix the first one of which consists of set,get,setinv,getinc
##library(MASS) calculates inverse for non squared and squared matrices
library(MASS)
makeCacheMatrix <- function(x = matrix()) {
        inv<-NULL
        set<-function(y){
                x<<-y
                inv<<-NULL
                }
        get<-function()x                     #function to get matrix x
        setinv<-function(inverse)inv<<-inverse
        getinv<-function(){
                invers<-ginv(x)
                invers%*%x                   #getting inverse of matrix
                }
        list(set = set,get = get,
             setinv = setinv,
             getinv = getinv)
        }



## Write a short comment describing this function
#this is used to get cached data

cacheSolve <- function(x, ...) {
        inv<- x$getinv()
        if(!is.null(inv)){
                message("getting cached data!")
                return(inv)
        }
        data<- x$get()
        inv<- solve(data, ...)
        x$setinv(inv)
        inv
     }
 
        ## Return a matrix that is the inverse of 'x'
}
