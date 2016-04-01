## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        myinver<-NULL
        set<-function(y){
                y<<-x
                myinver<<-NULL
                print(y)
        }
        get<-function() x
        getinv<-function() myinver
        setinv<-function(invmatrix) myinver<-invmatrix
        list(set = set,get=get,getinv=getinv,setinv=setinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        myinver<-x$getinv()
        if(!is.null(myinver))
        {
                Print("Pulled from Cache")
                return(myinver)
        }
        mymatrix<-x$get()
        myinver<-solve(mymatrix)
        x$setinv(myinver)
        myinver
}
