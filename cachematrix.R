# The functions below caches the inverse of a matrix.

# This function makes a list that contains a function to set a matrix,
# a function to get the matrix, a function to set the inverse of the matrix,
# and a function to get the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
        i<-NULL
        set<-function(y){
                x<<-y
                i<<-NULL
        }
        get<-function()x
        setinverse<-function(inverse) i<<-inverse
        getinverse<-function()i
        list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


# This function gives the inverse of a given matrix.
# If the inverse has already been computed, then computation is skipped
# and the inverse will be returned.  If the inverse has not been computed,
# then the function computes the inverse and returns it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i<-x$getinverse()
        if(!is.null(i)){
                message("Getting cached data")
                return(i)
        }
        data<-x$get()
        i<-solve(data)
        x$setinverse(i)
        i
}
