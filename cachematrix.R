## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        mInverse <-NULL
        
        set <- function(y = matrix()){
                x <<- y
                mInverse <<- NULL
        }
        
        get <- function(){x}
        
        setInverse <- function(tempInverse){
                mInverse <<-tempInverse
        }
        
        getInverse <- function(){
                mInverse
        }
        
        list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invMat <-x$getInverse()
        if(!is.null(invMat)){
                message("Getting Inverse Matrix from Cache!!")
                return(invMat)
        }
        
        origMatrix<-x$get()
        invMat <- solve(origMatrix)
        x$setInverse(invMat)
        invMat
}
