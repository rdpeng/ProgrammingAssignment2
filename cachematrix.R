## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        j<<-NULL
        get<-function(){
                x
        }
        setinv<-function(i){
                j<<-i
        }
        getinv<-function(){
                j
        }
        list(get=get,setinv=setinv,getinv=getinv)
}


## Write a short comment describing this function

cacheSolve <- function(ml, ...) {
        ## Return a matrix that is the inverse of 'x'
        i<-ml$getinv()
        if(!is.null(i)){
                message("getting cached inverse")
                return(i)
        }
        x<-ml$get()
        if(det(x)!=0){
                i<-solve(x)
                ml$setinv(i)
                return(i)        
        }
        message("This matrix is not invertible")
}
