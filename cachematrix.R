## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## This function ciontains nested functions that take an input of a matrix, invert it, 
## and caches the result for the cacheSolve function to retrive it later (by pushing m to the parent frame).

makeCacheMatrix <- function(x = matrix()) {
        m<-NULL  ## clears m value
        set<-function(y){
                x<<-y  ## pushes x out to parent frame to later be retrieved by get()
                m<<-NULL  ## resets the m value from a prior run
        }
        get<-function() x  ## retrives original matrix
        setmatrix<-function(solve) m<<- solve  ## caches the inverted matrix after solve()
        getmatrix<-function() m  ## retrives cached inverted matrix
        
        list(set=set, get=get,  ## prints the nested funtions above to aid learning process
             setmatrix=setmatrix,
             getmatrix=getmatrix)
}


## Write a short comment describing this function
## This function inverts a matrix if called fo rthe first time, or retrives the cached 
## matrix m if it has been defined


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m<-x$getmatrix()  ## trys to retirve cached inverted matrix
        if(!is.null(m)){  ## if m is not NULL, returns m after printing message
                message("getting cached data")
                return(m)
        }
        matrix<-x$get()  ## retrives original matrix
        m<-solve(matrix, ...)  ## inverst matrix
        x$setmatrix(m)  ## caches inverted matrix in m for later retirval
        m  ## returns m
}
