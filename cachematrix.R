## Put comments here that give an overall description of what your
## functions do

## In the below function I have deffined set and get functions 
## for the original matrix and also for the Inverse matrix; 
## finally a list is returned that contains all the functions

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


## In the below function I basically just check if the Inverse matrix have been
## already calculated; if that is the case I just retrieve it; otherwise
## further operations are done in order to get the inverse matrix and save it

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
