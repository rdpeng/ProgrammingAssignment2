## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##  makeCacheMatrics function is for caching the matrix and its inverse where:
## get funcion: gets the matrics value
## set function: resets the value of the matrics and the inverse
## getinverse function: gets the value of the inverse
## setinverse function: sets the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    inv_x<-matrix() #inv_x is the variable to store the matrics inverse and here is has one NA value 
    set<-function(new_matrix)
        {
        x<<-new_matrix
        inv_x<<-matrix()
        }

    get<-function() x
    setinverse <-function(inverse) inv_x<<-inverse 
    getinverse <- function() inv_x
    list(get=get,set=set,setinverse=setinverse,getinverse=getinverse)
}


## Write a short comment describing this function
## cacheSolve function calculates the Inverse of a square matrics if it is a new matrics if it was
##calculated before it will return the cached matrix :)
cacheSolve <- function(x, ...) {
    inv_x<-x$getinverse()
    if ( !is.na(inv_x))
        {
        message("getting cached Inverse Matrix")
        return(inv_x)
        }
    x_mat<-x$get()
   inv_x<-solve(x_mat)
   x$setinverse(inv_x)
   inv_x
        ## Return a matrix that is the inverse of 'x'
}
