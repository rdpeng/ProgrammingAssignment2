## Put comments here that give an overall description of what your
## functions do

## MakeCacheMatrix takes a matrix as its argument and...
## 1) resets the inverse matrix in the cache to NULL
## 2) creates a list of functions, linked to that matrix:
# - 'get' allows to retreive the matrix
# - 'setinv' allows to cache the inverse of the matrix as 'j', once 'cacheSolve' has calculated it.
# - 'getinv' retreives the inverse of the matrix from the cache

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
        list(get = get,
             setinv = setinv,
             getinv = getinv)
}


## cacheSolve takes as argument a list of functions of the form 'makeCacheMatrix'
## Its behavior depends on the following conditions:
## - If the inverse of that matrix has already been calculated, it is retreived from the cache
## - If it has not been calculated, the function checks if it is invertible
##     - If it isn't, it returns the message "This matrix is not invertible"
##     - If it is, the inverse is calculated, stored in the cache and returned

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
