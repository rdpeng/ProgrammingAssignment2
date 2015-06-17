## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
}
## Given an ordinary R matrix x, the "constructor" function makeCacheMatrix 
## creates an "object" that stores x. 
makeCacheMatrix <- function(x = matrix()) {
        
        ## Initially, the local variable xInv lacks a value.
        xInv <- NULL
        
        ## The set function assigns a value to the local variable xInv and
        ## assigns reassigns to x the value of the variable y.
        set <- function(y) {
                ## Within the body of the set function, x is a free variable. It
                ## is resolved in the enclosing environment of the function
                ## makeCacheMatrix.
                x <<- y
                ## Within the body of the set function, xInv is also a free 
                ## variable. It's value is resolved in the enclosing context 
                ## of makeCacheMatrix. If the value of x has been (re)set, 
                ## then the previous value of xInv is no longer valid and is 
                ## nullified.  
                xInv <<- NULL
        }
        
        ## The get function returns the current value associated with the symbol
        ## x.
        get <- function() x
        
        ## The set_xInv function assigns to xInv the value of the variable that
        ## is bound to the formal parameter 'inverse' when set_xInv is called.
        set_xInv <- function(inverse) {
                xInv <<- inverse 
        }  
        
        ## The get_xInv functions returns the current, possibly NULL, value of 
        ## the local variable xInv.
        get_xInv <- function() {
                xInv      
        } 
        
        ## The list directive, in effect, makes public the interface to the 
        ## functions defined in makeCacheMatrix. 
        list(set = set, get = get, set_xInv = set_xInv, get_xInv = get_xInv)
}

## The function cacheSolve either computes or fetches the inverse of a matrix 
## that is encapsulated by a makeCacheMatrix object denoted by x.
cacheSolve <- function(x, ...) {
        
        # Assign a value to the local variable xInv 
        xInv <- x$get_xInv()
        
        if (is.null(xInv)) {
                # message("computing inverse")
                m <- x$get()
                ## use a standard R function to compute the inverse
                xInv <- solve(m, ...)
                ## set the value of XInv in the makeCacheMatrix object 
                ## referred to by x.
                x$set_xInv(xInv)
                xInv
        } else {
                message("getting cached inverse")
                return(xInv)
        }
}