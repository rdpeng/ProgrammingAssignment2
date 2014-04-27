## These functions; MakeCacheMatrix and cacheSolve work together to assign, or set and get values 
## to the inverse of a matrix. Let's do MakeCacheMatrix first. 
## 

## MakeCacheMatrix basically is a function that takes a Matrix as an input and returns a list of fuctions. 
## In this case the list has 2 functions only; get and set. Get fetches the inverse of the input matrix. Set assigns
## the value of the inverse of the input matrix to a variable.
## Note: the function solve() is an R generated function to get the inverse of a matrix.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL # All this did is set up an empty variable, m, to store the cache
        
        set <- function(y){
                x <<- y # Here, y, a matrix input, is being assigned to x. The <<- is to ensure that x is available 
                        # outside of this 'set' function. The input matrix, now called x, is now available to other functions.
                m <<- NULL # Again, the <<- is used to make m, the cache for the inverse of y or any other input matrix,
                           # available outside of this function.
        }
        
        get <- function() {
                x       # This merely 'gets' of returns the value of x. We can 'get' it because we used '<<-' previously
                        # while assigning the input matrix to x. The <<- makes it available here.
        }
        
        setinverse <- function(anysquarematrix) {
                m <<- anysquarematrix # The solve() function computes for the inverse of the supplied square matrix.
        }                     # solve() actually takes 2 matrices as args but if only one is given, the other is assumed
                              # to be the identity matrix
        
        getinverse <- function() {
                m       # This 'gets' the inverse of the input matrix which has been been assigned to m
        }
        
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse) # This is the 'Return' value

}




## This function solves for the inverse of any supplied square matrix. It checks the cache for the calculated inverse.
## If it does not find the inverse, then and only then does it solve for the inverse.

cacheSolve <- function(x, ...) {
                m <- x$getinverse() # This calls the getinverse() function from the MakeCacheMatrix function.
                if(!is.null(m)) { # checking to see if the cache, 'm', is empty or not.
                        message("Getting cached Inverse")
                        return(m)
                }
                                
                anysquarematrix2 <- x$get()
                m <- solve(anysquarematrix2) # The solve() function computes for the inverse of the supplied square matrix.
                # solve() actually takes 2 matrices as args but if only one is given, the other is assumed
                # to be the identity matrix
                x$setinverse(m) # This calls the setinverse() function from the MakeCacheMatrix function and assigns m.
                m # m now stores the inverse of the supplied, 'anysquarematrix' and returns it.
        }        
