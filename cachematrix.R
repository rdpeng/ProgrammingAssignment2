## Write a short comment describing this function
##gettheinverse is a function that returns the vector x stored in the main function.
##inversemarix is a function that changes the vector stored in the main function.
##setmean and getmean are functions very similar to set and get.
##They don’t calculate the mean, they simply store the value of the input in a variable m.
##into the main function makeVector (setmean) and return it (getmean).
        
        
makeCacheMatrix <- function(x = matrix()) {
        m<-NULL
        SET<- FUNCTION(Y){
                
                X<<-Y
M<<-NULL }
        GET<-FUNCTION() X
        INVERSEMATRIX<-FUNCTION(SOLVEMATRIX)
        M<<-SOLVEMATRIX
        GETTHEINVERSE<-FUNCTION() M
        LIST(SET=SET, GET=GET,INVERSEMATRIX=INVERSEMATRIX,GETTHEINVERSE=GETTHEINVERSE)
        
}


## Write a short comment describing this function
## This function calculates the inverse of the special “matrix” 
##(which is the input of cachemean) returned by makeCacheMatrix.
##If the inverse has already been calculated then the cachesolve should retrieve the inverse from the cache. 
##inverse has not been calculated, data gets the matrix stored with makeCacheMatrix, m calculates the inverse, 
##and x$setmean(m) stores it in the object m in makeCacheMatrix.

cacheSolve <- function(x, ...) {
        M<-X$GETTHEINVERSE()
        IF (!IS.NULL(M)
            { MESSAGE("DATA FROM CACHE")
             RETURN(M)
             }
            DATA<-X$GET()
            M<-SOLVEMATRIX(DATA,...)
            X$INVERSEMATRIX(M)
            M
        ## Return a matrix that is the inverse of 'x'
}
