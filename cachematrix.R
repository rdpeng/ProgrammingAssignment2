## Caching the Inverse of a Matrix
## functions do

##  Creates a square matrix

makeCacheMatrix <- function(x = matrix(1:4,2,2)) {
  m<-NULL
        set<-function(y){
                x<<-y
                m<<-NULL
        }
        get<-function() x
        setmatrix<-function(solve) m<<- solve
        getmatrix<-function() m
        list(set=set, get=get,
             setmatrix=setmatrix,
             getmatrix=getmatrix)
}


## This function computes the inverse of the special "matrix"

## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x=matrix(), ...) {
        m<-x$getmatrix()
        if(!is.null(m)){
                message("Cache the inverse of a matrix")
                return(m)
        }
        matrix<-x$get()
        m<-solve(matrix, ...)
        x$setmatrix(m)
        m       
}
