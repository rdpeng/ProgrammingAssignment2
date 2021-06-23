## Put comments here that give an overall description of what your
## functions do


## Simplemente configuro la entrada x como una matriz, luego establezco el valor resuelto "s" como nulo y luego cambié todas las referencias 
## de "mean a "solve" 

makeCacheMatrix <- function(x = matrix()) {
        i<-NULL
        set<-function(matrix){
        m<<-matrix
        i<<-NULL
        }
        get<-function(){
        m
        }
        sinverse<-function(inverse){
        i<<-inverse
        }
        ginverse<-function(){
        i
        }
        list(set=set,get=get,
            sinverse=sinverse,
            ginverse=ginverse)

}

## Lo mismo aquí, cambiando "mean" por "solve" y "m" por "s"

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m<-x$ginverse()
        if(!is.null(m)){
        message("getting cached data")
        return(m)
        }
        data<-x$get()
        m<-solve(data)%*%data
        x$sinverse(m)
        m
}
