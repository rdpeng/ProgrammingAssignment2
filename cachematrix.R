## This creates a function that holds other functions, as well a matrix and its
## inverse. During function call, or with the use of the set() function, the inv
## is set to NULL to avoid mismatch between matrix and inverse. The other, public 
## functions defined in makeCacheMatrix are returned as a list.

makeCacheMatrix <- function(x = matrix()) {
    inv<-NULL ##n ulls inv during instantization. 
    set<-function(y) {
        x<<- y ## Change the value of x in makeCacheMatrix() environment
        ## to y. If the assignment had been ('<-'), then it would have only
        ## modified the local x. The x in makeCacheMatrix environment would 
        ## have remained unmodified. 
        
        inv<<- NULL ##If the matrix created by the function call is modified,
        ## this wipes out the preexisting inverse to avoid mismatch. 
    }
    get<-function() x ##simple function that returns the matrix. 
    
    setinv<- function(inverse) inv<<-inverse ##modifes the inv varibale 
    ## in makeCacheMatrix
    
    getinv<-function() inv 
    
    ## returns a list of functions.
    list(set=set, get=get,setinv=setinv,getinv=getinv) 

}


## This function returns a matrix that is the inverse of 'x'. It first looks up
## the cache for the inverse via a getinv() method in makeCacheMatrix(). If there
## is no inverse cached for x, the function sets said inverse via the setter
## method in makeCacheMatrix(), and returns the inverse at the end.

cacheSolve <- function(x,...) { ##Function uses subsetting. Arugment-type needs
    ## to include getter and setter methods. I.E., you cannot pass atomic data-types
    ## to this function. The ... in the example code was to accomodate arguments to 
    ## mean, and it functions similarly here (though I don't really 
    ## know what viable arguments can be passed to solve())
    
    inv <- x$getinv() 
    if(!is.null(inv)) { ##If an inverse already exists, the function prints it out
        ##and returns to the calling environment without invoking solve().
        message("getting cached data")
        return(inv)
    }
    m<- x$get() ##If Inverse is not yet cached, m is assigned a copy (by value)
    ## of the matrix set in the 'Special matrix'.
    
    inv<- solve(m,...) ##Inverses matrix.
    
    x$setinv(inv) ##The function assigns to the inv variable in the special matrix
    ## the value of the computed inverse.
    
    inv ## returns inverse. 
}
