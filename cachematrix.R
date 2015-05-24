## This creates a function that holds other functions, as well a matrix and its
##inverse. During function call, or with the use of the set() function, the inv
##is set to NULL to avoid mismatch between matrix and inverse. The other, public 
##functions defined in makeCacheMatrix as retuened as a list (this is why 
##cacheSolve has a ... after x in the function arguments.)

makeCacheMatrix <- function(x = matrix()) {
    inv<-NULL ##NULLS inv during instantization. 
    set<-function(y) {
        x<<- y ##Change the value of x in makeCacheMatrix to y. 
        inv<<- NULL ##If the matrix created by the function call is modified,
        ##this wipes out the preexisting inverse to avoid mismatch. 
    }
    get<-function() x ##simple function that returns the matrix. 
    setinv<- function(inverse) inv<<-inverse ##modifes the inv varibale 
    ##in makeCacheMatrix
    getinv<-function() inv 
    
    ##returns a list of functions.
    list(set=set, get=get,setinv=setinv,getinv=getinv) 

}


## This function returns a matrix that is the inverse of 'x'. It first looks up
## the cache for the inverse via a getinv() method in makeCacheMatrix(). If there
## is no inverse cached for x, the function sets said inverse via the setter
## method in makeCacheMatrix(), and returns the inverse at the end.

cacheSolve <- function(x, ...) { ##Function uses subsetting. Arugment-type needs
    ##to include getter and setter methods. I.E., you cannot pass atomic data-types
    ##to this function. 
    
    inv <- x$getinv() 
    if(!is.null(inv)) { ##If an inverse already exists, the function prints it out
        ##reutning to the calling environment without invoking solve().
        message("getting cached data")
        return(inv)
    }
    m<- x$get() ##If Inverse is not yet cached, m is assigned a copy (by value)
    ##of the matrix in the 'Special matrix'.
    inv<- solve(m)
    x$setinv(inv) ##The function assigns the inv variable in the special matrix
    ##the value of the computed inverse.
    inv
}
