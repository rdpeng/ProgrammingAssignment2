## This function can cache the inverse of a matrix

# Function makeCacheMatrix
#1. sets the vlaue of the matrix
#2. get the values of the matrix
#3. assigns the inverse of the matrix
#4. gets the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
      
      inv <- NULL
        set<-function(y){
        x   <<- y
        inv <<- NULL
        }
          
      get   <- function()x
      setinv<- function(solve) inv<<-solve
      getinv<- function() inv
      list(set=set,get=get,setinv = setinv,getinv = getinv)
      

}


## CacheSolve function function compuets the inverse of the matrix
## returned by the function makeCacheMatrix. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      
      inv <- x$getinv()
      
      if(!is.null(inv)){
        message("getting catched data")
        return(inv)
      
      }
      
      data<- x$get()
      inv <- solve(data, ...)
      x$setinv(inv)
      inv
}
