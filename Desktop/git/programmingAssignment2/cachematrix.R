## create a special matrix object, a function that checks the inverse already computed & stored in cache
## if yes, value stored in cache is returned. else, a new value is calculated, stored in cahce for future reference & returned 

## ver2.0 - practice git

## 'makeCacheMatrix' takes a 'regular matrix argument' 
## & returns list of fucntions 
makeCacheMatrix <- function(x = matrix()) {
    inverse<-NULL

    getMatrix <- function() x
    getInverse <- function() inverse
    
    setMatrix <- function(y) {
        x<<-y
        inverse <<-NULL
    }
    setInverse <- function(inv) inverse <<- inv
    
    list(set=setMatrix,get=getMatrix,setInverse=setInverse,getInverse=getInverse)
}


## 'cacheSolve' takes a 'list argument' (each element is a pointer to function) 
## & returns the inverse of a matrix, before checking if the inverse if alreday calculated before

cacheSolve <- function(x, ...) {
    i<-x$getInverse()
    if(!is.null(i)) {
        message("getting the cached result")
        return(i)
    }
    data<-x$get()
    i<-solve(data)
    x$setInverse(i)
    i
}
