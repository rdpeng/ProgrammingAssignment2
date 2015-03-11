## this is a function to cache the inverse of a matrix

## makeCacheMatrix is a function to creat special 'matrix' objec that can cache its inverse
##the function will return a list that contains four functions to
## 1. set the matrix
## 2. get the matrix
## 3. set the inverse of the matrix
## 4. get the inverse of the matrix
## this list can be used as the input to the 'cacheSolve' function to cache its inverse
makeCacheMatrix<-function(x=matrix()){
        
        m<-NULL
        # set the matrix x to a new matrix y and reset the matrix m to NULL
        set<-function(y){
                x<<-y
                m<<-NULL
        }
        # return the matrix x
        get<-function() x
        # set the inverse matrix to z
        setinverse<-function(inverse) m<<-inverse
        # return the inverse matrix z
        getinverse<-function() m
        #return the list contains the four functions
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}
##cacheSolve is a function to compute the inverse of the special 'matrix' returned by 'makeCacheMatrix'
## if the inverse has been calculated and the input matrix has not changed, it will retrieve the inverse from the cache 
cacheSolve<-function(x, ...){
        m<-x$getinverse()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        data<-x$get()
        m<-solve(data, ...)
        x$setinverse(m)
        ## return a matrix that is the inverse of 'x'
        return(m)
}