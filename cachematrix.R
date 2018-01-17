## Put comments here that give an overall description of what your
## functions do
## This funtion intizalizes  a matrix and sets the cash value of its
## inverse to null.
## This funtion defines sub functions that sets the inverse once computed
## and returns it or a null value

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

    m<- NULL
    set<- function(y){
       x<<-y
      m<<-NULL
    }
    get<- funtion() x
    setinverse<- funtion(inverse) m<<-inverse
    getinverse<- funtion() m
    list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


## Write a short comment describing this function
## Return a matrix that is the inverse of  Matrix 'x'
## This function examines the content of the cash.
## If the inverse was prevuiously calculated, it returns the cashed value
## Otherwise it computes the inverse, stores it in the cash and returns it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m<-x$getinverse()
        if(!is.null(m)){
                message("getting casHed Matric Inverse"
                return(m)
        }
        data<-x$get()
        m<-solve(data,...)
        x$setinverse(m)
        m
}
