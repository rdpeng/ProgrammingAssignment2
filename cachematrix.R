## Effectively this code sets up a new type of object with 
# to supplement the matrix class. The purpose is to
# cache the inverse of the matrix along with the original
# data, thereby reducing time wasteted on repeated inversion.
#This special matrtrix uses lexical scoping and is implemented
#as a list  of functions


#When an invertible square matrix is input this function returns
#the list of functions needed to deal with the special matrix.
#The task of acutually calculating the inverse is handed off 
#to the cacheSolve, but repeat calls to cacheSolve will
#retrieve the saved inverse.  Initial call will look like
#x<-makeCacheMatrix(y).

# 1) The set function can only be used on a properly formatted
# list conforming to this standard.  If x is such an object,
# call as x$set(y), where y is revised matrix

# 2) Similarly for existing formatted x, call of
# x$get will return the stored matrix value

# 3) For existing x, call of x$setinv(y) will attach the 
# matrix y to the structure for use as the inverse.
# But this isn't inteded to be called directly.  See
# cacheSolve.

# 4) For existing x, call of x$getinv will retrieve a 
#previously cached copy of the inverse.
makeCacheMatrix <- function(x = matrix()) {
        inv<-NULL
        set<-function(y){
                x<<-y
                inv<<-NULL
        }
        get<-function() x
        setinv<-function(inverse) inv<<-inverse
        getinv<-function()inv
        list(set=set,get=get,setinv=setinv,getinv=getinv)
}


## If x is a properly setup special matrix then when
## cacheSolve(x) is called it will retrieve the 
## cached value for the inverse of the function, if
## it has already been found through use of the x$getinv(),
## see 4) above.
## or it will calculate the inverse and store using x$setinv(y)
## described in 3) above, and return this inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv<-x$getinv()
        if(!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        data<-x$get()
        inv<-solve(data,...)
        x$setinv(inv)
        inv
}
