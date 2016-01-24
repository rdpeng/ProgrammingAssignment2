## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## Assignment 3 
makeCacheMatrix <- function(x = matrix()) {

    #set the value of the vector
    inv_matrix <- NULL
   
   set <- function(y) {
        x <<- y
        inv_matrix <<- NULL
    }
   
   #get the value of the vector
   get = function() x
    
    #set the value of the mean
    setinv = function(inverse) inv_matrix <<- inverse

    #get the value of the mean    
    getinv = function() inv_matrix
    
    list(set=set, get=get, setinv=setinv, getinv=getinv)


}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        #get  inverse from ca
        inv_matrix <-x$getinv()
        if(!is.null(inv_matrix)) {
            message("getting cached data")
            return(inv_matrix)
        }
        data <-x$get()
        inv_matrix <-solve(data,...)
        
        x$setinv(inv_matrix)
        inv_matrix
        
}





